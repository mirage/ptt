open Rresult
open Lwt.Infix

let src = Logs.Src.create "ptt.mti-gf"

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
  module Relay = Ptt.Relay.Make (Stack)
  module Server = Ptt_server.Make (Time) (Stack)
  module Sendmail = Ptt_sendmail.Make (Pclock) (Stack) (Happy_eyeballs)

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
      | [ Ok ipv4; Ok ipv6 ] -> Ok [ ipv4; ipv6 ]
      | [ (Ok ipv4); Error _ ] -> Ok [ ipv4 ]
      | [ Error _; (Ok ipv6) ] -> Ok [ ipv6 ]
      | [ (Error _ as err); _ ] -> err
      | [] | [_] | _ :: _ :: _ -> assert false in
    { getmxbyname; gethostbyname }

  let server_job ~pool ?stop ~port stack dns server close =
    let handler flow =
      let ipaddr, port = Stack.TCP.dst flow in
      Lwt.finalize
        (fun () ->
          Lwt_pool.use pool @@ fun (encoder, decoder, queue) ->
          Relay.accept ~encoder:(Fun.const encoder) ~decoder:(Fun.const decoder)
            ~queue:(Fun.const queue) ~ipaddr flow dns resolver server
          >|= R.reword_error (R.msgf "%a" Relay.pp_error))
        (fun () -> Stack.TCP.close flow)
      >>= function
      | Ok () -> Lwt.return ()
      | Error (`Msg err) ->
        Log.err (fun m -> m "<%a:%d> raised an error: %s" Ipaddr.pp ipaddr port err);
        Lwt.return () in
    Server.init ~port stack >>= fun service ->
    Server.serve_when_ready ?stop ~handler service
    |> fun (`Initialized job) ->
    let job = job >|= close in job

  let logic_job ~info map (ic, oc) =
    let sender =
      let local = `Dot_string [ "ptt"; "mti-gf" ] in
      Some (Colombe.Path.{ local; domain= info.Ptt_common.domain; rest= [] }) in
    let rec go () =
      Lwt_stream.get ic >>= function
      | None -> oc None; Lwt.return_unit
      | Some (key, stream, wk) ->
        let recipients = Ptt.Msgd.recipients key in
        let recipients = List.map fst recipients in
        let recipients = Ptt_map.expand ~info map recipients in
        let recipients = Ptt_aggregate.to_recipients ~info recipients in
        let id = Ptt_common.id_to_messageID ~info (Ptt.Msgd.id key) in
        let elts = List.map (fun recipients ->
          (* TODO(dinosaure): Can we use multiple MAIL FROM to keep the original
             sender? We actually force <ptt.mti-gf@info.Ptt_common.domain> to be
             SPF-valid in front of SMTP servers even if we are not the original
             author of the email. The original author is kept into the [Sender]
             field of the email which is unchanged. *)
          { Ptt_sendmail.sender
          ; recipients
          ; data= Lwt_stream.clone stream
          ; policies= []
          ; id }) recipients in
        List.iter (oc $ Option.some) elts;
        Lwt.wakeup_later wk `Ok;
        Lwt.pause () >>= go in
    go ()

  let job ?(limit = 20) ?stop ~locals ~port ~tls ~info stack dns he =
    let pool0 =
      Lwt_pool.create limit @@ fun () ->
      let encoder = Bytes.create 0x7ff in
      let decoder = Bytes.create 0x7ff in
      let queue = Ke.Rke.create ~capacity:0x800 Bigarray.char in
      Lwt.return (encoder, decoder, queue) in
    let pool1 =
      Lwt_pool.create limit @@ fun () ->
      let encoder = Bytes.create 0x7ff in
      let decoder = Bytes.create 0x7ff in
      let queue = Ke.Rke.create ~capacity:0x800 Bigarray.char in
      Lwt.return (encoder, decoder, queue) in
    let pool1 =
      { Ptt_sendmail.pool= fun fn -> Lwt_pool.use pool1 fn } in
    let ic_server, stream0, close0 = Relay.create ~info in
    let oc_server, push0 = Sendmail.v ~resolver ~pool:pool1 ~info tls in
    Lwt.join
      [ server_job ~pool:pool0 ?stop ~port stack dns ic_server close0
      ; logic_job ~info locals (stream0, push0)
      ; Sendmail.job dns he oc_server ]
end
