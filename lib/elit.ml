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
    (Dns_client : Dns_client_mirage.S)
    (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow) =
struct
  module Server = Ptt_server.Make (Time) (Stack)
  module Sendmail = Ptt_sendmail.Make (Pclock) (Stack) (Happy_eyeballs)
  module Nss = Ca_certs_nss.Make (Pclock)
  module Uspf_client = Uspf_mirage.Make (Dns_client)

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
        | Some (key, stream, wk) ->
          Lwt.catch 
            (fun () ->
              let sender = fst (Ptt.Msgd.from key) in
              let recipients = Ptt.Msgd.recipients key in
              let recipients = List.map fst recipients in
              let recipients = Ptt_map.expand ~info map recipients in
              let recipients = Ptt_aggregate.to_recipients ~info recipients in
              let id = Ptt_common.id_to_messageID ~info (Ptt.Msgd.id key) in
              Log.debug (fun m -> m "%a submitted a new email %a."
                Colombe.Reverse_path.pp sender Mrmime.MessageID.pp id);
              let elts = List.map (fun recipients ->
                { Ptt_sendmail.sender
                ; recipients
                ; data= Lwt_stream.clone stream
                ; policies= []
                ; id }) recipients in
              Log.debug (fun m -> m "Notice the SMTP server that everything is ok for %a from %a."
                Colombe.Reverse_path.pp sender Mrmime.MessageID.pp id);
              Lwt.wakeup_later wk `Ok;
              Log.debug (fun m -> m "Send the incoming email %a to our destination."
                Mrmime.MessageID.pp id);
              List.iter (oc $ Option.some) elts;
              Lwt.return_unit)
          (fun exn ->
            Log.err (fun m -> m "Got an error into the submission logic: %S" (Printexc.to_string exn));
            Lwt.return_unit)
          >>= Lwt.pause >>= go in
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
        | [ Ok ipv4; Ok ipv6 ] -> Ok [ ipv4; ipv6 ]
        | [ Error _; (Ok ipv6) ] -> Ok [ ipv6 ]
        | [ (Ok ipv4); Error _ ] -> Ok [ ipv4 ]
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

    let stream_of_field (field_name : Mrmime.Field_name.t) unstrctrd =
      Lwt_stream.of_list
        [ (field_name :> string)
        ; ": "
        ; Unstrctrd.to_utf_8_string unstrctrd; "\r\n" ]

    let forward_granted ipaddr allowed_to_forward =
      List.exists (fun prefix -> Ipaddr.Prefix.mem ipaddr prefix) allowed_to_forward

    let only_registered_recipients ~info map recipients =
      let for_all = function
        | Colombe.Forward_path.Postmaster -> true
        | Domain domain' -> Colombe.Domain.equal info.Ptt_common.domain domain'
        | Forward_path { Colombe.Path.local; domain= domain'; _ } ->
          Colombe.Domain.equal info.Ptt_common.domain domain'
          && Ptt_map.exists ~local map in
      List.for_all for_all recipients

    let verify ~info ~sender ~ipaddr dns stream =
      let ctx =
        Uspf.empty
        |> Uspf.with_ip ipaddr
        |> fun ctx -> Option.fold ~none:ctx
          ~some:(fun v -> Uspf.with_sender (`MAILFROM v) ctx)
          sender in
      Uspf_client.get ~ctx dns >>= function
      | Error _ -> Lwt.return (`Requested_action_not_taken `Permanent)
      | Ok record ->
        Uspf_client.check ~ctx dns record >>= fun result ->
        let receiver = match info.Ptt_common.domain with
          | Colombe.Domain.Domain ds -> `Domain ds
          | IPv4 ipv4 -> `Addr (Emile.IPv4 ipv4)
          | IPv6 ipv6 -> `Addr (Emile.IPv6 ipv6)
          | Extension (k, v) -> `Addr (Emile.Ext (k, v)) in
        let field_name, unstrctrd = Uspf.to_field ~ctx ~receiver result in
        let stream = Lwt_stream.append (stream_of_field field_name unstrctrd) stream in
        Lwt.return (`Ok stream)

    let mail_exchange_logic_job ~info ~map ~allowed_to_forward dns (ic, oc) =
      let sender =
        let local = `Dot_string [ "ptt"; "elit" ] in
        Some (Colombe.Path.{ local; domain= info.Ptt_common.domain; rest= [] }) in
      let rec go () =
        Lwt_stream.get ic >>= function
        | None -> oc None; Lwt.return_unit
        | Some (key, stream, wk) ->
          let id = Ptt_common.id_to_messageID ~info (Ptt.Msgd.id key) in
          Log.debug (fun m -> m "%a sent a new email %a to: @[<hov>%a@]."
            Colombe.Reverse_path.pp sender Mrmime.MessageID.pp id
            Fmt.(Dump.list Colombe.Forward_path.pp) (List.map fst (Ptt.Msgd.recipients key)));
          let fake_recipients = Ptt.Msgd.recipients key in
          let fake_recipients = List.map fst fake_recipients in
          let real_recipients = Ptt_map.expand ~info map fake_recipients in
          Log.debug (fun m -> m "real recipients of %a: @[<hov>%a@]"
            Mrmime.MessageID.pp id
            Fmt.(Dump.list Colombe.Forward_path.pp) real_recipients);
          let real_recipients = Ptt_aggregate.to_recipients ~info real_recipients in
          begin
            if forward_granted (Ptt.Msgd.ipaddr key) allowed_to_forward
            then Lwt.return (`Ok stream)
            else verify ~info
              ~sender:(fst (Ptt.Msgd.from key))
              ~ipaddr:(Ptt.Msgd.ipaddr key) dns stream end >>= function
          | #Ptt.Msgd.error as err ->
            Log.warn (fun m -> m "Can not verify SPF informations from %a for %a, discard it!"
              Colombe.Reverse_path.pp (fst (Ptt.Msgd.from key))
              Mrmime.MessageID.pp id);
            Lwt.wakeup_later wk err;
            Lwt.pause () >>= go
          | `Ok stream ->
            let elts = List.map (fun recipients ->
              { Ptt_sendmail.sender
              ; recipients
              ; data= Lwt_stream.clone stream
              ; policies= []
              ; id }) real_recipients in
            let src = Ptt.Msgd.ipaddr key in
            if forward_granted src allowed_to_forward
            || only_registered_recipients ~info map fake_recipients
            then begin
              List.iter (oc $ Option.some) elts;
              Log.debug (fun m -> m "Notice the SMTP server that everything is ok for %a from %a (%a)."
                Mrmime.MessageID.pp id
                Colombe.Reverse_path.pp (fst (Ptt.Msgd.from key))
                Ipaddr.pp (Ptt.Msgd.ipaddr key));
              Lwt.wakeup_later wk `Ok
            end else begin
              Log.warn (fun m -> m "Email %a to unknown users (%a), discard it!"
                Mrmime.MessageID.pp id
                Fmt.(Dump.list Colombe.Forward_path.pp) fake_recipients);
              Lwt.wakeup_later wk (`Requested_action_not_taken `Permanent)
            end;
            Lwt.pause () >>= go in
      go ()

    let job ?(limit = 20) ?stop ~locals ?port ~tls ~info ?(forward_granted= []) stack dns he =
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
      let allowed_to_forward = forward_granted in
      Lwt.join
        [ mail_exchange_job ~pool:pool0 ?stop ?port stack dns ic_server close0
        ; mail_exchange_logic_job ~info ~map:locals ~allowed_to_forward dns (stream0, push0)
        ; Sendmail.job dns he oc_server ]
  end

  type 'k t =
    { locals : Ptt_map.t
    ; tls : Tls.Config.client
    ; random : Mirage_crypto_rng.g option
    ; hash : 'k Digestif.hash
    ; authentication : 'k Ptt.Authentication.t
    ; mechanisms : Ptt.Mechanism.t list
    ; destination : Ipaddr.t
    ; forward_granted : Ipaddr.Prefix.t list }

  type 'k iter = (Ptt_map.local -> 'k Digestif.t -> Emile.mailbox list -> unit Lwt.t) -> unit Lwt.t

  let v ?g ?(mechanisms= [ Ptt.Mechanism.PLAIN ]) ~postmaster ?(forward_granted= []) hash iter destination =
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
    { locals; tls; random= g; hash; authentication; mechanisms; destination
    ; forward_granted }

  let job ?stop t ~info ?submission ?relay stack dns he =
    if Option.is_some info.Ptt_common.tls
    then Log.warn (fun m -> m "Discard the TLS server configuration from the [info] value");
    let submission = { info with Ptt_common.tls= submission } in
    let relay = { info with Ptt_common.tls= relay } in
    Lwt.join
      [ Local.job ?stop ~locals:t.locals ~tls:t.tls ~info:submission ~destination:[ t.destination ]
          stack he t.random t.hash t.authentication t.mechanisms
      ; Out.job ?stop ~locals:t.locals ~tls:t.tls ~info:relay ~forward_granted:t.forward_granted
          stack dns he ]
end
