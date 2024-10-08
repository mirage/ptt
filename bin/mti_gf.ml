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
  Mti_gf.Make (Time) (Mclock) (Pclock) (Resolver) (Tcpip_stack_socket.V4V6)

let load_file filename = Bos.OS.File.read filename

let cert =
  let open Rresult in
  load_file (Fpath.v "ptt.pem") >>= fun raw -> X509.Certificate.decode_pem raw

let cert = Rresult.R.get_ok cert

let private_key =
  let open Rresult in
  load_file (Fpath.v "ptt.key") >>= fun raw -> X509.Private_key.decode_pem raw

let private_key = Rresult.R.get_ok private_key

let tls =
  let authenticator = R.failwith_error_msg (Ca_certs.authenticator ()) in
  R.failwith_error_msg (Tls.Config.client ~authenticator ())

let fiber ~domain locals =
  let open Lwt.Infix in
  let open Tcpip_stack_socket.V4V6 in
  let ipv4_only = false and ipv6_only = false in
  TCP.connect ~ipv4_only ~ipv6_only Ipaddr.V4.Prefix.global None
  >>= fun tcpv4v6 ->
  let info =
    {
      Ptt.SMTP.domain
    ; Ptt.SMTP.ipaddr= Ipaddr.(V4 V4.any)
    ; Ptt.SMTP.tls= None
    ; Ptt.SMTP.zone= Mrmime.Date.Zone.GMT
    ; Ptt.SMTP.size= 0x1000000L
    } in
  let he = Happy_eyeballs_lwt.create () in
  let resolver = Dns_client_lwt.create he in
  Server.fiber ~port:4242 ~locals ~tls tcpv4v6 resolver info

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
