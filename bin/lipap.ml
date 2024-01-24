let () = Printexc.record_backtrace true
let reporter = Logs_fmt.reporter ()
let () = Fmt.set_utf_8 Fmt.stdout true
let () = Fmt.set_utf_8 Fmt.stderr true
let () = Fmt.set_style_renderer Fmt.stdout `Ansi_tty
let () = Fmt.set_style_renderer Fmt.stderr `Ansi_tty
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Logs.set_reporter reporter
let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)
let ( <.> ) f g x = f (g x)

module Random = struct
  type g = unit

  let generate ?g:_ len =
    let ic = open_in "/dev/urandom" in
    let rs = Bytes.create len in
    really_input ic rs 0 len ; close_in ic ; Cstruct.of_bytes rs
end

open Rresult

module Resolver = struct
  type +'a io = 'a Lwt.t
  type t = Dns_client_lwt.t

  let gethostbyname t v =
    let open Lwt.Infix in
    Dns_client_lwt.gethostbyname t v >|= function
    | Ok v -> Ok (Ipaddr.V4 v)
    | Error _ as err -> err

  let getmxbyname t v =
    let open Lwt_result in
    Dns_client_lwt.getaddrinfo t Dns.Rr_map.Mx v >|= fun (_, mxs) -> mxs

  let extension _t _ldh _v =
    Lwt.return (R.error_msgf "Impossible to resolve [%s:%s]" _ldh _v)
end

module Server =
  Lipap.Make (Random) (Time) (Mclock) (Pclock) (Resolver)
    (Tcpip_stack_socket.V4V6)

let load_file filename =
  let open Rresult in
  Bos.OS.File.read filename >>= fun contents ->
  R.ok (Cstruct.of_string contents)

let cert =
  let open Rresult in
  load_file (Fpath.v "server.pem") >>= fun raw ->
  X509.Certificate.decode_pem raw

let cert = Rresult.R.get_ok cert

let private_key =
  let open Rresult in
  load_file (Fpath.v "server.key") >>= fun raw ->
  X509.Private_key.decode_pem raw

let private_key = Rresult.R.get_ok private_key

let authenticator _username _password =
  Ptt_tuyau.Lwt_backend.Lwt_scheduler.inj (Lwt.return true)

let authenticator = Ptt.Authentication.v authenticator

let fiber ~domain locals =
  let open Lwt.Infix in
  let open Tcpip_stack_socket.V4V6 in
  let ipv4_only = false and ipv6_only = false in
  let tls =
    Tls.Config.server
      ~certificates:(`Single ([cert], private_key))
      ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None)
      () in

  TCP.connect ~ipv4_only ~ipv6_only Ipaddr.V4.Prefix.global None
  >>= fun tcpv4v6 ->
  let info =
    {
      Ptt.SMTP.domain
    ; Ptt.SMTP.ipaddr= Ipaddr.(V4 V4.any)
    ; Ptt.SMTP.tls= Some tls
    ; Ptt.SMTP.zone= Mrmime.Date.Zone.GMT
    ; Ptt.SMTP.size= 0x1000000L
    } in
  let resolver = Dns_client_lwt.create () in
  let tls =
    let authenticator = R.failwith_error_msg (Ca_certs.authenticator ()) in
    Tls.Config.client ~authenticator () in

  Server.fiber ~port:4242 ~locals ~tls tcpv4v6 resolver None Digestif.BLAKE2B
    info authenticator [Ptt.Mechanism.PLAIN]

let romain_calascibetta =
  let open Mrmime.Mailbox in
  Local.[w "romain"; w "calascibetta"] @ Domain.(domain, [a "gmail"; a "com"])

let () =
  let domain = Domain_name.(host_exn <.> of_string_exn) "x25519.net" in
  let locals = Ptt.Relay_map.empty ~postmaster:romain_calascibetta ~domain in
  let locals =
    let open Mrmime.Mailbox in
    Ptt.Relay_map.add
      ~local:Local.(v [w "romain"; w "calascibetta"])
      romain_calascibetta locals in
  Lwt_main.run (fiber ~domain locals)
