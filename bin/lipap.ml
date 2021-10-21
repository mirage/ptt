let () = Memtrace.trace_if_requested ()
let () = Printexc.record_backtrace true
let reporter = Logs_fmt.reporter ()
let () = Fmt.set_utf_8 Fmt.stdout true
let () = Fmt.set_utf_8 Fmt.stderr true
let () = Fmt.set_style_renderer Fmt.stdout `Ansi_tty
let () = Fmt.set_style_renderer Fmt.stderr `Ansi_tty
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Logs.set_reporter reporter
(* let () = Mirage_crypto_rng_unix.initialize () *)
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

  let gethostbyname t v = Dns_client_lwt.gethostbyname t v

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

let authenticator _username _password =
  Ptt_tuyau.Lwt_backend.Lwt_scheduler.inj (Lwt.return true)

let authenticator = Ptt.Authentication.v authenticator

let tls =
  let authenticator = R.failwith_error_msg (Ca_certs.authenticator ()) in
  Tls.Config.client ~authenticator ()

let fiber cert private_key ~domain map =
  let open Lwt.Infix in
  let open Tcpip_stack_socket.V4V6 in
  let ipv4_only = false and ipv6_only = false in
  TCP.connect ~ipv4_only ~ipv6_only Ipaddr.V4.Prefix.global None
  >>= fun tcpv4 ->
  UDP.connect ~ipv4_only ~ipv6_only Ipaddr.V4.Prefix.global None
  >>= fun udpv4 ->
  connect udpv4 tcpv4 >>= fun stackv4 ->
  let info =
    {
      Ptt.SMTP.domain
    ; Ptt.SMTP.ipv4= Ipaddr.V4.any
    ; Ptt.SMTP.tls=
        Tls.Config.server
          ~certificates:(`Single ([cert], private_key))
          ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None)
          ()
    ; Ptt.SMTP.zone= Mrmime.Date.Zone.GMT
    ; Ptt.SMTP.size= 0x1000000L
    } in
  let resolver = Dns_client_lwt.create () in
  Server.fiber ~port:4242 ~tls stackv4 resolver None Digestif.BLAKE2B map info
    authenticator [Ptt.Mechanism.PLAIN]

let postmaster =
  let open Mrmime.Mailbox in
  Local.[ w "postmaster" ] @ Domain.(domain, [ a "osau"; a "re" ])

let () = match Sys.argv with
  | [| _; certificate; private_key; |] ->
    let certificate, private_key =
      let open Rresult in begin
      Fpath.of_string certificate >>= fun certificate ->
      Fpath.of_string private_key >>= fun private_key ->
      load_file certificate >>= fun certificate ->
      X509.Certificate.decode_pem certificate >>= fun certificate ->
      load_file private_key >>= fun private_key ->
      X509.Private_key.decode_pem private_key >>= fun private_key ->
      R.ok (certificate, private_key) end |> R.failwith_error_msg in
    let domain = Domain_name.(host_exn <.> of_string_exn) "osau.re" in
    let map = Ptt.Relay_map.empty ~postmaster ~domain in
    Lwt_main.run (fiber certificate private_key ~domain map)
  | _ -> Fmt.epr "%s <certificate> <private-key>\n%!" Sys.argv.(0)
