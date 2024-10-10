open Rresult
open Lwt.Infix

let src = Logs.Src.create "ptt.hm"

module Log : Logs.LOG = (val Logs.src_log src)

let ( $ ) f g = fun x -> f (g x)

module Make
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    (Dns_client : Dns_client_mirage.S)
    (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow) =
struct
  module Verifier = Ptt.Relay.Make (Stack)
  module Server = Ptt_server.Make (Time) (Stack)
  module Sendmail = Ptt_sendmail.Make (Pclock) (Stack) (Happy_eyeballs)
  module Uspf_client = Uspf_mirage.Make (Dns_client)

  let resolver =
    let open Ptt_common in
    let getmxbyname dns domain_name =
      Dns_client.getaddrinfo dns Dns.Rr_map.Mx domain_name
      >|= Result.map snd in
    let gethostbyname dns domain_name =
      let ipv4 =
        Dns_client.gethostbyname dns domain_name
        >|= Result.map (fun ipv4 -> Ipaddr.V4 ipv4) in
      let ipv6 =
        Dns_client.gethostbyname6 dns domain_name
        >|= Result.map (fun ipv6 -> Ipaddr.V6 ipv6) in
      Lwt.all [ ipv4; ipv6 ] >|= function
      | [ _; (Ok _ as ipv6) ] -> ipv6
      | [ (Ok _ as ipv4); Error _ ] -> ipv4
      | [ (Error _ as err); _ ] -> err
      | [] | [_] | _ :: _ :: _ -> assert false in
    { getmxbyname; gethostbyname }

  let server_job ~pool ?stop ~port stack dns server close =
    let handler flow =
      let ipaddr, port = Stack.TCP.dst flow in
      Lwt.finalize
        (fun () ->
          Lwt_pool.use pool @@ fun (encoder, decoder, queue) ->
          Verifier.accept ~encoder:(Fun.const encoder)
            ~decoder:(Fun.const decoder) ~queue:(Fun.const queue) ~ipaddr flow
            dns resolver server
          >|= R.reword_error (R.msgf "%a" Verifier.pp_error)
          >>= fun res ->
          Stack.TCP.close flow >>= fun () -> Lwt.return res)
        (fun () -> Stack.TCP.close flow)
      >>= function
      | Ok () -> Lwt.return ()
      | Error (`Msg err) ->
        Log.err (fun m -> m "<%a:%d> %s" Ipaddr.pp ipaddr port err);
        Lwt.return () in
    Server.init ~port stack >>= fun service ->
    Server.serve_when_ready ?stop ~handler service
    |> fun (`Initialized job) ->
    let job = job >|= close in job

  let stream_of_field (field_name : Mrmime.Field_name.t) unstrctrd =
    Lwt_stream.of_list
      [ (field_name :> string)
      ; ": "
      ; Unstrctrd.to_utf_8_string unstrctrd; "\r\n" ]

  let logic_job ~info map (ic, oc) dns =
    let rec go () =
      Lwt_stream.get ic >>= function
      | None -> oc None; Lwt.return_unit
      | Some (key, stream) ->
        let sender, _ = Ptt.Messaged.from key in
        let ctx =
          Uspf.empty
          |> Uspf.with_ip (Ptt.Messaged.ipaddr key)
          |> fun ctx -> Option.fold ~none:ctx
            ~some:(fun sender -> Uspf.with_sender (`MAILFROM sender) ctx)
            sender in
        let verify () =
          Uspf_client.get ~ctx dns >>= function
          | Error (`Msg msg) ->
            Log.err (fun m -> m "Got an error from SPF: %s" msg);
            (* TODO(dinosaure): add a new field into the incoming email. *)
            Lwt.return stream
          | Ok record ->
            Uspf_client.check ~ctx dns record >>= fun result ->
            let receiver = match info.Ptt_common.domain with
              | Colombe.Domain.Domain ds -> `Domain ds
              | IPv4 ipv4 -> `Addr (Emile.IPv4 ipv4)
              | IPv6 ipv6 -> `Addr (Emile.IPv6 ipv6)
              | Extension (k, v) -> `Addr (Emile.Ext (k, v)) in
            let field_name, unstrctrd = Uspf.to_field ~ctx ~receiver result in
            let stream = Lwt_stream.append (stream_of_field field_name unstrctrd) stream in
            Lwt.return stream in
        verify () >>= fun stream ->
        let recipients = Ptt.Messaged.recipients key in
        let recipients = List.map fst recipients in
        let recipients = Ptt_map.expand ~info map recipients in
        let recipients = Ptt_aggregate.to_recipients ~info recipients in
        let id = Ptt_common.id_to_messageID ~info (Ptt.Messaged.id key) in
        let elts = List.map (fun recipients ->
          { Ptt_sendmail.sender
          ; recipients
          ; data= Lwt_stream.clone stream
          ; policies= []
          ; id }) recipients in
        List.iter (oc $ Option.some) elts;
        Lwt.pause () >>= go in
    go ()

  let job ?(limit = 20) ?stop ~locals ~port ~tls ~info stack dns he =
    let pool0 =
      Lwt_pool.create limit @@ fun () ->
      let encoder = Bytes.create Colombe.Encoder.io_buffer_size in
      let decoder = Bytes.create Colombe.Decoder.io_buffer_size in
      let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
      Lwt.return (encoder, decoder, queue) in
    let pool1 =
      Lwt_pool.create limit @@ fun () ->
      let encoder = Bytes.create Colombe.Encoder.io_buffer_size in
      let decoder = Bytes.create Colombe.Decoder.io_buffer_size in
      let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
      Lwt.return (encoder, decoder, queue) in
    let pool1 =
      { Ptt_sendmail.pool= fun fn -> Lwt_pool.use pool1 fn } in
    let ic_server, stream0, close0 = Verifier.create ~info in
    let oc_server, push0 = Sendmail.v ~resolver ~pool:pool1 ~info tls in
    Lwt.join
      [ server_job ~pool:pool0 ?stop ~port stack dns ic_server close0
      ; logic_job ~info locals (stream0, push0) dns
      ; Sendmail.job dns he oc_server ]
end
