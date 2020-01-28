let () = Printexc.record_backtrace true
let reporter = Logs_fmt.reporter ()
let () = Fmt.set_utf_8 Fmt.stdout true
let () = Fmt.set_utf_8 Fmt.stderr true
let () = Fmt.set_style_renderer Fmt.stdout `Ansi_tty
let () = Fmt.set_style_renderer Fmt.stderr `Ansi_tty
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Logs.set_reporter reporter

let ( <.> ) f g = fun x -> f (g x)
let apply x f = f x

open Lwt_backend
open Rresult
open Lwt.Infix

module Relay = Ptt.Relay.Make(Lwt_scheduler)(Lwt_io)

module Relay_map = struct
end

module Mxs = Ptt.Mxs
module By_domain = Ptt.Aggregate.By_domain
module By_ipaddr = Ptt.Aggregate.By_ipaddr

type resolved =
  | Ipaddr of Ipaddr.t
  | Domain of [ `host ] Domain_name.t * Mxs.t

module Resolved = Map.Make(struct
    type t = resolved

    let compare a b = match a, b with
      | Ipaddr a, Ipaddr b -> Ipaddr.compare a b
      | Domain (_, mxs_a), Domain (_, mxs_b) ->
        let { Mxs.mx_ipaddr= a; _ } = Mxs.choose mxs_a in
        let { Mxs.mx_ipaddr= b; _ } = Mxs.choose mxs_b in
        Ipaddr.compare a b
      | Ipaddr a, Domain (_, mxs) ->
        let { Mxs.mx_ipaddr= b; _ } = Mxs.choose mxs in
        Ipaddr.compare a b
      | Domain (_, mxs), Ipaddr b ->
        let { Mxs.mx_ipaddr= a; _ } = Mxs.choose mxs in
        Ipaddr.compare a b
  end)

module Mclock = struct
  let elapsed_ns = Mtime_clock.elapsed_ns
  let period_ns = Mtime_clock.period_ns
end

module Random = struct
  type g = unit

  let generate ?g:_ len =
    let ic = open_in "/dev/urandom" in
    let rs = Bytes.create len in
    really_input ic rs 0 len ; close_in ic ;
    Cstruct.of_bytes rs
end

module Make (StackV4 : Mirage_stack.V4) = struct
  module TCP = Tuyau_mirage_tcp.Make(StackV4)
  module TLS = Tuyau_mirage_tls
  module DNS = Dns_client_mirage.Make(Random)(Mclock)(StackV4)

  let src = Logs.Src.create "MTI-PF"
  module Log = ((val Logs.src_log src) : Logs.LOG)

  let tls_endpoint, tls_protocol = TLS.protocol_with_tls ~key:TCP.endpoint TCP.protocol
  let tls_configuration, tls_service = TLS.service_with_tls ~key:TCP.configuration TCP.service tls_protocol

  let ( >>? ) x f = x >>= function
    | Ok x -> f x
    | Error err -> Lwt.return (Error err)

  let ( >|? ) x f = x >>= function
    | Ok x -> Lwt.return (f x)
    | Error err -> Lwt.return (Error err)

  let dns_impl =
    { Relay.gethostbyname= (fun w v -> DNS.gethostbyname w v)
    ; Relay.getmxbyname= (fun w v -> DNS.getaddrinfo w Dns.Rr_map.Mx v >>= function
        | Ok (_, v) -> Lwt.return (Ok v)
        | Error _ as err -> Lwt.return err)
    ; Relay.extension= (fun _ldh _v -> Lwt.return (Rresult.R.error_msgf "Impossible to resolve [%s:%s]" _ldh _v)) }

  let failwithf fmt = Fmt.kstrf (fun err -> Failure err) fmt

  let rdwr_of_flow
    : type flow. (module Tuyau_mirage.FLOW with type flow = flow) -> (flow, Lwt_scheduler.t) Colombe.Sigs.rdwr
    = fun (module Flow) ->
      (* XXX(dinosaure): we must use [Cstruct.t] instead [Bytes.t] at the end. *)
    let ic_raw = Cstruct.create 0x1000 in
    let oc_raw = Cstruct.create 0x1000 in
    let rd flow buf off len =
      let len = min len (Cstruct.len ic_raw) in
      let ic_raw = Cstruct.sub ic_raw 0 len in
      let rec fiber = function
        | Ok `End_of_input -> Lwt.return 0
        | Ok (`Input 0) -> Flow.recv flow ic_raw >>= fiber
        | Ok (`Input len) ->
          Cstruct.blit_to_bytes ic_raw 0 buf off len ;
          Lwt.return len
        | Error err -> Lwt.fail (failwithf "%a" Flow.pp_error err) in
      Lwt_scheduler.inj (Flow.recv flow ic_raw >>= fiber) in
    let rec wr flow buf off len =
      let n = min len (Cstruct.len oc_raw) in
      Cstruct.blit_from_string buf off oc_raw 0 n ;
      Flow.send flow (Cstruct.sub oc_raw 0 n) >>= function
      | Ok n ->
        if n = len then Lwt.return () else wr flow buf (off + n) (len - n)
      | Error err -> Lwt.fail (failwithf "%a" Flow.pp_error err) in
    let wr flow buf off len = Lwt_scheduler.inj (wr flow buf off len) in
    { Colombe.Sigs.rd; Colombe.Sigs.wr; }

  let plug_consumer_to_producers consumer producers =
    let rec go () = consumer () >>= function
      | Some v ->
        List.iter (fun producer -> producer (Some v)) producers ; Lwt.pause () >>= go
      | None ->
        List.iter (fun producer -> producer None) producers ; Lwt.return () in
    go

  let sendmail stack conf_server mx_ipaddr emitter producer recipients =
    let endpoint =
      { Tuyau_mirage_tcp.stack= stack
      ; Tuyau_mirage_tcp.keepalive= None
      ; Tuyau_mirage_tcp.ip= mx_ipaddr
      ; Tuyau_mirage_tcp.port= 25 } in
    Tuyau_mirage.flow_of_protocol ~key:TCP.endpoint endpoint ~protocol:TCP.protocol >>? fun flow ->
    let ctx = Sendmail_with_tls.Context_with_tls.make () in
    let rdwr = rdwr_of_flow (Tuyau_mirage.impl_of_flow TCP.protocol) in
    let tls = Tls.Config.client ~authenticator:X509.Authenticator.null () in
    let domain =
      let vs = Domain_name.to_strings (Relay.info conf_server).Ptt.SMTP.domain in
      Colombe.Domain.Domain vs in
    Lwt.catch
      (fun () ->
         Sendmail_with_tls.sendmail lwt rdwr flow ctx tls ~domain emitter recipients producer
         |> Lwt_scheduler.prj >|= R.reword_error (fun err -> `Sendmail err))
      (function Failure err -> Lwt.return (R.error_msg err) (* XXX(dinosaure): should come from [rdwr]. *)
              | exn -> Lwt.return (Error (`Exn exn))) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error (`Sendmail err) ->
      Lwt.return (R.error_msgf "%a" Sendmail_with_tls.pp_error err)
    | Error (`Msg _) as err ->
      Lwt.return err
    | Error (`Exn exn) ->
      Log.err (fun m -> m "Got an exception: %s" (Printexc.to_string exn)) ;
      Lwt.return (Error (`Msg "Unknown exception"))

  let transmit stack resolver conf_server map (key, queue, consumer) =
    let info = Relay.info conf_server in
    let impl = Relay.resolver conf_server in
    let unresolved, resolved =
      Ptt.Messaged.recipients key |> List.map fst |>
      Ptt.Aggregate.aggregate_by_domains ~domain:info.Ptt.SMTP.domain in
    let unresolved, resolved = Ptt.Relay_map.expand map unresolved resolved in
    let fold resolved (domain, v) =
      impl.getmxbyname resolver domain >>= function
      | Error (`Msg err) ->
        Log.err (fun m -> m "Domain %a does not have a Mail Exchange service: %s" Domain_name.pp domain err) ;
        Lwt.return resolved
      | Ok set ->
        let fold mxs { Dns.Mx.mail_exchange; Dns.Mx.preference; } =
          impl.gethostbyname resolver mail_exchange >>= function
          | Ok mx_ipaddr -> Lwt.return Mxs.(add (v ~preference ~domain:mail_exchange (Ipaddr.V4 mx_ipaddr)) mxs)
          | Error (`Msg err) ->
            Log.warn (fun m -> m "Impossible to reach the Mail Exchange service %a: %s" Domain_name.pp mail_exchange err ) ;
            Lwt.return mxs in
        Lwt_list.fold_left_s fold Mxs.empty (Dns.Rr_map.Mx_set.elements set) >>= fun mxs ->
        if Mxs.is_empty mxs
        then ( Log.err (fun m -> m "Domain %a does not have a reachable Mail Exchange service." Domain_name.pp domain )
             ; Lwt.return resolved )
        else
          let postmaster = [ `Atom "Postmaster" ] in
          let { Mxs.mx_ipaddr; _ } = Mxs.choose mxs in
          match v with
          | `All -> Lwt.return (Resolved.add (Domain (domain, mxs)) `All resolved)
          | `Local l0 ->
            ( match Resolved.find_opt (Ipaddr mx_ipaddr) resolved with
              | None -> Lwt.return (Resolved.add (Domain (domain, mxs)) (`Local l0) resolved)
              | Some `All -> Lwt.return resolved
              | Some (`Local l1) ->
                let l = List.sort_uniq (Emile.compare_local ~case_sensitive:true) (l0 @ l1) in
                Lwt.return (Resolved.add (Domain (domain, mxs)) (`Local l) resolved)
              | Some `Postmaster ->
                Lwt.return (Resolved.add (Domain (domain, mxs)) (`Local (postmaster :: l0)) resolved) )
          | `Postmaster -> match Resolved.find_opt (Ipaddr mx_ipaddr) resolved with
            | Some `Postmaster | Some `All -> Lwt.return resolved
            | Some (`Local l) -> Lwt.return (Resolved.add (Domain (domain, mxs)) (`Local (postmaster :: l)) resolved)
            | None -> Lwt.return (Resolved.add (Domain (domain, mxs)) (`Local [ postmaster ]) resolved) in
    let resolved = By_ipaddr.fold (fun k v m -> Resolved.add (Ipaddr k) v m) resolved Resolved.empty in
    Lwt_list.fold_left_s fold resolved (By_domain.bindings unresolved) >>= fun resolved ->
    let resolved = Resolved.bindings resolved in
    let producers, targets =
      List.fold_left (fun (producers, targets) target ->
          let stream, producer = Lwt_stream.create () in
          let stream () = Lwt_scheduler.inj (Lwt_stream.get stream) in
          (producer :: producers), (stream, target) :: targets)
        ([], []) resolved in
    let transmit = plug_consumer_to_producers consumer producers in
    let emitter, _ = Ptt.Messaged.from key and id = Ptt.Messaged.id key in
    let sendmails =
      let sendmail (stream, (k, vs)) () =
        let open Colombe in
        let open Forward_path in
        let mx_domain, mxs = match k with
          | Ipaddr ((Ipaddr.V4 v4) as mx_ipaddr) -> Domain.IPv4 v4, Mxs.(singleton (v ~preference:0 mx_ipaddr))
          | Ipaddr ((Ipaddr.V6 v6) as mx_ipaddr) -> Domain.IPv6 v6, Mxs.(singleton (v ~preference:0 mx_ipaddr))
          | Domain (mx_domain, mxs) -> Domain.Domain (Domain_name.to_strings mx_domain), mxs in
        let recipients = match vs with
          | `All -> [ Domain mx_domain ]
          | `Local vs ->
            List.map (fun local ->
                let local = `Dot_string (List.map (function `Atom x -> x | `String x -> x) local) in
                Forward_path { Path.local= local; Path.domain= mx_domain; Path.rest= []; }) vs in
        Log.info (fun m -> m "Start to send the email %Ld to %a" id Colombe.Domain.pp mx_domain) ;
        let rec go = function
          | [] ->
            Log.err (fun m -> m "Impossible to send the email %Ld to %a."
                        id Colombe.Domain.pp mx_domain) ;
            Lwt.return ()
          | { Mxs.mx_ipaddr= Ipaddr.V6 _; _ } :: rest -> go rest
            (* XXX(dinosaure): we completely ignore IPv6 when the current stack is only for V4. *)
          | { Mxs.mx_ipaddr= Ipaddr.V4 mx_ipaddr; _ } :: rest ->
            sendmail stack conf_server mx_ipaddr emitter stream recipients >>= function
            | Ok () ->
              Log.info (fun m -> m "Email %Ld correctly sended to domain %a [%a]"
                           id Colombe.Domain.pp mx_domain Ipaddr.V4.pp mx_ipaddr) ;
              Lwt.return ()
            | Error err ->
              Log.err (fun m -> m "Got an error while sending email %Ld to %a: %a"
                          id Ipaddr.V4.pp mx_ipaddr Tuyau_mirage.pp_error err) ;
              go rest in
        let sort = List.sort (fun { Mxs.preference= a; _ } { Mxs.preference= b; _ } -> Int.compare a b) in
        go (Mxs.elements mxs |> sort) in
      List.map sendmail targets in
    Lwt.join (List.map (apply ()) (transmit :: sendmails)) >>= fun () ->
    Log.info (fun m -> m "Transmission of the mail %Ld correctly done." id ) ;
    Relay.Md.close queue

  let smtp_relay_service resolver conf conf_server =
    Tuyau_mirage.impl_of_service ~key:TCP.configuration TCP.service |> Lwt.return >>? fun (module Server) ->
    Tuyau_mirage.serve ~key:TCP.configuration conf ~service:TCP.service >>? fun (t, protocol) ->
    let handle ip port (Tuyau_mirage.Flow (flow, (module Flow))) () =
      let rdwr = rdwr_of_flow (module Flow) in
      Lwt.catch
        (fun () -> Relay.accept rdwr flow resolver conf_server >|= R.reword_error (R.msgf "%a" Relay.pp_error) >>? fun () ->
          (* TODO(dinosaure): [Flow.close] is called only if [Relay.accept] returns [Ok]. *)
          Flow.close flow >|= R.reword_error (R.msgf "%a" Flow.pp_error) )
        (function Failure err -> Lwt.return (R.error_msg err)
                | exn -> Lwt.return (Error (`Exn exn))) >>= function
      | Ok () ->
        Log.info (fun m -> m "<%a:%d> submitted a message." Ipaddr.V4.pp ip port) ;
        Lwt.return ()
      | Error (`Msg err) ->
        Log.err (fun m -> m "<%a:%d> %s" Ipaddr.V4.pp ip port err) ;
        Lwt.return ()
      | Error (`Exn exn) ->
        Log.err (fun m -> m "<%a:%d> raised an unknown exception: %s" Ipaddr.V4.pp ip port (Printexc.to_string exn)) ;
        Lwt.return () in
    let rec go () =
      Server.accept t >>? fun flow ->
      let ip, port = TCP.dst flow in
      let flow = Tuyau_mirage.abstract protocol flow in
      Lwt.async (handle ip port flow) ; Lwt.pause () >>= go in
    go () >|= R.reword_error (R.msgf "%a" Server.pp_error)

  let smtp_relay_service resolver conf conf_server =
    smtp_relay_service resolver conf conf_server >>= function
    | Ok () -> Lwt.return ()
    | Error err ->
      Log.err (fun m -> m "%a" Tuyau_mirage.pp_error err) ;
      Lwt.return ()

  let smtp_logic stack resolver conf_server messaged map =
    let rec go () =
      Relay.Md.await messaged >>= fun () ->
      Relay.Md.pop messaged >>= function
      | None -> Lwt.pause () >>= go
      | Some v -> Lwt.async (fun () -> transmit stack resolver conf_server map v) ; Lwt.pause () >>= go in
    go ()

  let fiber stack resolver conf map info =
    let conf_server = Relay.create ~info dns_impl in
    let messaged = Relay.messaged conf_server in
    Lwt.join [ smtp_relay_service resolver conf conf_server
             ; smtp_logic stack resolver conf_server messaged map
             ; Nocrypto_entropy_lwt.initialize () ]
end

module Server = Make(Tcpip_stack_socket)

let load_file filename =
  let open Rresult in
  Bos.OS.File.read filename >>= fun contents ->
  R.ok (Cstruct.of_string contents)

let cert =
  let open Rresult in
  load_file (Fpath.v "ptt.pem") >>= fun raw ->
  X509.Certificate.decode_pem raw

let cert = Rresult.R.get_ok cert

let private_key =
  let open Rresult in
  load_file (Fpath.v "ptt.key") >>= fun raw ->
  X509.Private_key.decode_pem raw >>= fun (`RSA v) -> R.ok v

let private_key = Rresult.R.get_ok private_key

let fiber ~domain map =
  Tcpip_stack_socket.TCPV4.connect None >>= fun tcpv4 ->
  Tcpip_stack_socket.UDPV4.connect None >>= fun udpv4 ->
  Tcpip_stack_socket.connect [] udpv4 tcpv4 >>= fun stackv4 ->
  let conf =
    { Tuyau_mirage_tcp.stack= stackv4
    ; Tuyau_mirage_tcp.keepalive= None
    ; Tuyau_mirage_tcp.port= 4242 } in
  let info =
    { Relay.domain
    ; Relay.ipv4= Ipaddr.V4.any
    ; Relay.tls= Tls.Config.server ~certificates:(`Single ([ cert ], private_key)) ~authenticator:X509.Authenticator.null ()
    ; Relay.size= 0x1000000L } in
  let resolver = Server.DNS.create stackv4 in
  Server.fiber stackv4 resolver conf map info

let romain_calascibetta =
  let open Mrmime.Mailbox in
  Local.[ w "romain"; w "calascibetta" ] @ Domain.(domain, [ a "gmail"; a "com" ])

let () =
  let domain = Domain_name.(host_exn <.> of_string_exn) "x25519.net" in
  let map = Ptt.Relay_map.empty ~postmaster:romain_calascibetta ~domain in
  let map =
    let open Mrmime.Mailbox in
    Ptt.Relay_map.add
      ~local:(Local.(v [ w "romain"; w "calascibetta" ]))
      romain_calascibetta map in
  Lwt_main.run (fiber ~domain map)
