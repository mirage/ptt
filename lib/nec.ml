open Rresult
open Lwt.Infix

let src = Logs.Src.create "ptt.nec"

module Log : Logs.LOG = (val Logs.src_log src)

let ( $ ) f g = fun x -> f (g x)

module Make
    (Stack : Tcpip.Stack.V4V6)
    (Dns_client : Dns_client_mirage.S)
    (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow) =
struct
  module Signer = Ptt.Relay.Make (Stack)
  module Server = Ptt_server.Make (Stack)
  module Sendmail = Ptt_sendmail.Make (Stack) (Happy_eyeballs)
  module DKIM = Dkim_mirage.Make (Dns_client)

  let resolver =
    let open Ptt_common in
    let getmxbyname dns domain_name =
      Dns_client.getaddrinfo dns Dns.Rr_map.Mx domain_name >|= Result.map snd
    in
    let gethostbyname dns domain_name =
      let ipv4 =
        Dns_client.gethostbyname dns domain_name
        >|= Result.map (fun ipv4 -> Ipaddr.V4 ipv4) in
      let ipv6 =
        Dns_client.gethostbyname6 dns domain_name
        >|= Result.map (fun ipv6 -> Ipaddr.V6 ipv6) in
      Lwt.all [ipv4; ipv6] >|= function
      | [Ok ipv4; Ok ipv6] -> Ok [ipv4; ipv6]
      | [Ok ipv4; Error _] -> Ok [ipv4]
      | [Error _; Ok ipv6] -> Ok [ipv6]
      | [(Error _ as err); _] -> err
      | [] | [_] | _ :: _ :: _ -> assert false in
    {getmxbyname; gethostbyname}

  let server_job ~pool ?stop ~port stack dns server close =
    let handler flow =
      let ipaddr, port = Stack.TCP.dst flow in
      Lwt.finalize
        (fun () ->
          Lwt_pool.use pool @@ fun (encoder, decoder, queue) ->
          Signer.accept ~encoder:(Fun.const encoder)
            ~decoder:(Fun.const decoder) ~queue:(Fun.const queue) ~ipaddr flow
            dns resolver server
          >|= R.reword_error (R.msgf "%a" Signer.pp_error))
        (fun () -> Stack.TCP.close flow)
      >>= function
      | Ok () -> Lwt.return ()
      | Error (`Msg err) ->
        Log.err (fun m -> m "<%a:%d> %s" Ipaddr.pp ipaddr port err);
        Lwt.return () in
    Server.init ~port stack >>= fun service ->
    Server.serve_when_ready ?stop ~handler service |> fun (`Initialized job) ->
    let job = job >|= close in
    job

  let request dns domain_name =
    Dns_client.getaddrinfo dns Dns.Rr_map.Txt domain_name >>= function
    | Ok (_ttl, txts) ->
      let txts = Dns.Rr_map.Txt_set.to_list txts in
      let txts = List.map (String.concat "" $ String.split_on_char ' ') txts in
      let txts = String.concat "" txts in
      begin
        match Dkim.domain_key_of_string txts with
        | Ok dk -> Lwt.return (domain_name, `Domain_key dk)
        | Error (`Msg msg) -> Lwt.return (domain_name, `DNS_error msg)
      end
    | Error (`Msg msg) -> Lwt.return (domain_name, `DNS_error msg)

  let verify_and_sign ~info dns ~pk seal msgsig dkim stream =
    let receiver =
      match info.Ptt_common.domain with
      | Colombe.Domain.Domain ds -> `Domain ds
      | IPv4 ipv4 -> `Addr (Emile.IPv4 ipv4)
      | IPv6 ipv6 -> `Addr (Emile.IPv6 ipv6)
      | Extension (k, v) -> `Addr (Emile.Ext (k, v)) in
    let stream0 = Lwt_stream.clone stream in
    let stream1 = Lwt_stream.clone stream in
    let stream2 = Lwt_stream.clone stream in
    let chain =
      let rec go decoder =
        match Arc.Verify.decode decoder with
        | `Queries (decoder, set) -> begin
          match Arc.Verify.queries set with
          | Error _ -> Lwt.return_error (`Invalid_domain_key set)
          | Ok queries ->
            Lwt_list.map_s (request dns) queries >>= fun responses ->
            let decoder = Arc.Verify.response decoder responses in
            let decoder = Result.get_ok decoder in
            go decoder
        end
        | `Await decoder -> begin
          Lwt_stream.get stream0 >>= function
          | Some str -> go (Arc.Verify.src decoder str 0 (String.length str))
          | None -> go (Arc.Verify.src decoder String.empty 0 0)
        end
        | `Malformed _ -> Lwt.return_error `Invalid_email
        | `Chain chain -> Lwt.return_ok chain in
      go (Arc.Verify.decoder ()) in
    let dkim =
      DKIM.sign ~key:pk ~newline:`CRLF dkim stream1 >>= function
      | Ok dkim ->
        let new_line = "\r\n" in
        let bbh = (Dkim.signature_and_hash dkim :> string * Dkim.hash_value) in
        let dkim = Dkim.with_signature_and_hash dkim bbh in
        Lwt.return_ok (Prettym.to_string ~new_line Dkim.Encoder.as_field dkim)
      | Error _ as err -> Lwt.return err in
    Lwt.both chain dkim >>= function
    | Error _, Error _ ->
      Log.err (fun m -> m "Impossible to sign (ARC & DKIM) the incoming email");
      Lwt.return stream
    | Error _, Ok dkim ->
      Log.err (fun m ->
          m "Impossible to add a new ARC-Set into the incoming email");
      let prefix = Lwt_stream.of_list [dkim] in
      Lwt.return (Lwt_stream.append prefix stream)
    | Ok chain, dkim -> begin
      let dkim =
        match dkim with
        | Ok dkim -> dkim
        | Error _ ->
          Log.err (fun m ->
              m "Impossible to add a DKIM signature into the incoming email");
          String.empty in
      let seal chain =
        let rec go t =
          match Arc.Sign.sign t with
          | `Malformed _ -> Lwt.return_error `Invalid_email
          | `Set set -> Lwt.return_ok set
          | `Await t -> begin
            Lwt_stream.get stream2 >>= function
            | Some str -> go (Arc.Sign.fill t str 0 (String.length str))
            | None -> go (Arc.Sign.fill t String.empty 0 0)
          end in
        go (Arc.Sign.signer ~seal ~msgsig ~receiver (pk, None) chain) in
      seal chain >>= function
      | Ok set ->
        let new_line = "\r\n" in
        let set = Prettym.to_string ~new_line Arc.Encoder.stamp set in
        let prefix = Lwt_stream.of_list [set; dkim] in
        Lwt.return (Lwt_stream.append prefix stream)
      | Error _ ->
        Log.err (fun m -> m "Impossible to get ARC sets");
        let prefix = Lwt_stream.of_list [dkim] in
        Lwt.return (Lwt_stream.append prefix stream)
    end

  let logic_job ~info dns map (ic, oc) (pk, dkim) =
    let hash =
      let (Dkim.Hash_algorithm hash) = Dkim.hash_algorithm dkim in
      match Digestif.hash_to_hash' hash with
      | #Dkim.hash as hash -> hash
      | _ -> assert false in
    let seal =
      Arc.Sign.seal ~algorithm:(Dkim.algorithm dkim) ~hash
        ~selector:(Dkim.selector dkim) (Dkim.domain dkim) in
    let rec go () =
      Lwt_stream.get ic >>= function
      | None -> oc None; Lwt.return_unit
      | Some (key, stream, wk) ->
        let sign_and_transmit () =
          Lwt.catch (fun () ->
              verify_and_sign ~info dns ~pk seal dkim dkim stream
              >>= fun stream ->
              let sender, _ = Ptt.Msgd.from key in
              let recipients = Ptt.Msgd.recipients key in
              let recipients = List.map fst recipients in
              let recipients = Ptt_map.expand ~info map recipients in
              let recipients = Ptt_aggregate.to_recipients ~info recipients in
              let id = Ptt_common.id_to_messageID ~info (Ptt.Msgd.id key) in
              let elts =
                List.map
                  (fun recipients ->
                    {
                      Ptt_sendmail.sender
                    ; recipients
                    ; data= Lwt_stream.clone stream
                    ; policies= []
                    ; id
                    })
                  recipients in
              List.iter (oc $ Option.some) elts;
              Lwt.wakeup_later wk `Ok;
              Lwt.return_unit)
          @@ fun exn ->
          Log.err (fun m ->
              m "Impossible to sign the incoming email: %S"
                (Printexc.to_string exn));
          Lwt.return_unit in
        sign_and_transmit () >>= Lwt.pause >>= go in
    go ()

  let job
      ?(limit = 20)
      ?stop
      ~locals
      ~port
      ~tls
      ~info
      stack
      dns
      he
      (private_key, dkim) =
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
    let pool1 = {Ptt_sendmail.pool= (fun fn -> Lwt_pool.use pool1 fn)} in
    let ic_server, stream0, close0 = Signer.create ~info in
    let oc_server, push0 = Sendmail.v ~resolver ~pool:pool1 ~info tls in
    Lwt.join
      [
        server_job ~pool:pool0 ?stop ~port stack dns ic_server close0
      ; logic_job ~info dns locals (stream0, push0) (private_key, dkim)
      ; Sendmail.job dns he oc_server
      ]
end
