let () = Printexc.record_backtrace true
let reporter = Logs_fmt.reporter ()
let () = Fmt.set_utf_8 Fmt.stdout true
let () = Fmt.set_utf_8 Fmt.stderr true
let () = Fmt.set_style_renderer Fmt.stdout `Ansi_tty
let () = Fmt.set_style_renderer Fmt.stderr `Ansi_tty
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Logs.set_reporter reporter
let () = Mirage_crypto_rng_unix.use_default ()
let ( <.> ) f g x = f (g x)

open Rresult

module Happy_eyeballs_daemon =
  Happy_eyeballs_mirage.Make (Tcpip_stack_socket.V4V6)

module Dns_client =
  Dns_client_mirage.Make (Tcpip_stack_socket.V4V6) (Happy_eyeballs_daemon)

module Server =
  Mti_gf.Make (Tcpip_stack_socket.V4V6) (Dns_client) (Happy_eyeballs_daemon)

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

let job ~domain locals =
  let open Lwt.Infix in
  let open Tcpip_stack_socket.V4V6 in
  let ipv4_only = false and ipv6_only = false in
  TCP.connect ~ipv4_only ~ipv6_only Ipaddr.V4.Prefix.global None
  >>= fun tcpv4v6 ->
  UDP.connect ~ipv4_only ~ipv6_only Ipaddr.V4.Prefix.global None
  >>= fun udpv4v6 ->
  connect udpv4v6 tcpv4v6 >>= fun stack ->
  let info =
    {
      Ptt.SMTP.domain
    ; Ptt.SMTP.ipaddr= Ipaddr.(V4 V4.any)
    ; Ptt.SMTP.tls= None
    ; Ptt.SMTP.zone= Mrmime.Date.Zone.GMT
    ; Ptt.SMTP.size= 0x1000000L
    } in
  Happy_eyeballs_daemon.connect_device stack >>= fun he ->
  let dns = Dns_client.create (stack, he) in
  Server.job ~port:4242 ~locals ~tls ~info tcpv4v6 dns he

let romain_calascibetta =
  let open Mrmime.Mailbox in
  Local.[w "romain"; w "calascibetta"] @ Domain.(domain, [a "gmail"; a "com"])

let () =
  let locals = Ptt_map.empty ~postmaster:romain_calascibetta in
  let domain = Colombe.Domain.Domain ["ptt"; "fr"] in
  Ptt_map.add
    ~local:(`Dot_string ["romain"; "calascibetta"])
    romain_calascibetta locals;
  Lwt_main.run (job ~domain locals)
