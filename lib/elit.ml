open Rresult
open Lwt.Infix

let src = Logs.Src.create "ptt.elit"

module Log : Logs.LOG = (val Logs.src_log src)

let ( $ ) f g = fun x -> f (g x)

module Make
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    (Dns_client : Dns_client_mirage.S
    (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow) =
struct
  module Server = Ptt_server.Make (Time) (Stack)
  module Sendmail = Ptt_sendmail.Make (Pclock) (Stack) (Happy_eyeballs)
  module Nss = Ca_certs_nss.Make (Pclock)

  module Local = struct
    module Submission = Ptt.Submission.Make (Stack)

    let submission_resolver =
      let open Ptt_common in
      let getmxbyname _ipaddr mail_exchange =
        Dns.Rr_map.Mx_set.(singleton { Dns.Mx.preference= 0; mail_exchange })
        |> Lwt.return_ok in
      let gethostbyname ipaddr _domain_name =
        Lwt.return_ok ipaddr in
      { getmxbyname; gethostbyname }

    let submission_job ~pool ?stop ?(port= 465) ~destination
      random hash stack server close =
      let handler flow =
        let ipaddr, port = Stack.TCP.dst flow in
        Lwt.finalize
          (fun () ->
            Lwt_pool.use pool @@ fun (encoder, decoder, _) ->
            Submission.accept_without_starttls
              ~encoder:(Fun.const encoder) ~decoder:(Fun.const decoder) ~ipaddr
              flow destination submission_resolver
              random hash server
            >|= R.reword_error (R.msgf "%a" Submission.pp_error))
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

    let submission_logic_job ~info map (ic, oc) =
      let rec go () =
        Lwt_stream.get ic >>= function
        | None -> oc None; Lwt.return_unit
        | Some (key, stream) ->
          let sender = fst (Ptt.Messaged.from key) in
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

    let job ?(limit = 20) ?stop ~locals ?port ~tls ~info ~destination
      stack he random hash authenticator mechanisms =
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
      let ic_server, stream0, close0 = Submission.create ~info ~authenticator mechanisms in
      let oc_server, push0 = Sendmail.v ~resolver:submission_resolver ~pool:pool1 ~info tls in
      Lwt.join
        [ submission_job ~pool:pool0 ?stop ?port ~destination random hash stack ic_server close0
        ; submission_logic_job ~info locals (stream0, push0)
        ; Sendmail.job destination he oc_server ]
  end

  module Out = struct
    module Relay = Ptt.Relay.Make (Stack)

    let mail_exchange_resolver =
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

    let mail_exchange_job ~pool ?stop ?(port= 25) stack dns server close =
      let handler flow =
        let ipaddr, port = Stack.TCP.dst flow in
        Lwt.finalize
          (fun () ->
            Lwt_pool.use pool @@ fun (encoder, decoder, queue) ->
            Relay.accept ~encoder:(Fun.const encoder) ~decoder:(Fun.const decoder)
              ~queue:(Fun.const queue) ~ipaddr flow dns mail_exchange_resolver server
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

    let mail_exchange_logic_job ~info map (ic, oc) =
      let sender =
        let local = `Dot_string [ "ptt"; "mti-gf" ] in
        Some (Colombe.Path.{ local; domain= info.Ptt_common.domain; rest= [] }) in
      let rec go () =
        Lwt_stream.get ic >>= function
        | None -> oc None; Lwt.return_unit
        | Some (key, stream) ->
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

    let job ?(limit = 20) ?stop ~locals ?port ~tls ~info stack dns he =
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
      let oc_server, push0 = Sendmail.v ~resolver:mail_exchange_resolver ~pool:pool1 ~info tls in
      Lwt.join
        [ mail_exchange_job ~pool:pool0 ?stop ?port stack dns ic_server close0
        ; mail_exchange_logic_job ~info locals (stream0, push0)
        ; Sendmail.job dns he oc_server ]
  end

  type 'k t =
    { locals : Ptt_map.t
    ; tls : Tls.Config.client
    ; random : Mirage_crypto_rng.g option
    ; hash : 'k Digestif.hash
    ; authentication : 'k Ptt.Authentication.t
    ; mechanisms : Ptt.Mechanism.t list
    ; destination : Ipaddr.t }

  type 'k iter = (Ptt_map.local -> 'k Digestif.t -> Emile.mailbox list -> unit Lwt.t) -> unit Lwt.t

  let v ?g ?(mechanisms= [ Ptt.Mechanism.PLAIN ]) ~postmaster hash iter destination =
    let authenticator = R.failwith_error_msg (Nss.authenticator ()) in
    let tls = Rresult.R.failwith_error_msg (Tls.Config.client ~authenticator ()) in
    let locals = Ptt_map.empty ~postmaster in
    let passwds = Hashtbl.create 0x100 in
    let add local passwd dsts =
      List.iter (fun dst -> Ptt_map.add ~local dst locals) dsts;
      Hashtbl.add passwds local passwd;
      Lwt.return_unit in
    iter add >|= fun () ->
    let authentication local passwd' =
      match Hashtbl.find_opt passwds local with
      | Some passwd -> Lwt.return (Digestif.equal hash passwd passwd')
      | None -> Lwt.return false in
    let authentication = Ptt.Authentication.v authentication in
    { locals; tls; random= g; hash; authentication; mechanisms; destination }

  let job ?stop t ~info stack dns he =
    Lwt.join
      [ Local.job ?stop ~locals:t.locals ~tls:t.tls ~info ~destination:t.destination
          stack he t.random t.hash t.authentication t.mechanisms
      ; Out.job ?stop ~locals:t.locals ~tls:t.tls ~info stack dns he ]
end
