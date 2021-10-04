open Mirage

let fields =
  let doc = Key.Arg.info ~doc:"List of fields to sign (separated by a colon)." [ "fields" ] in
  Key.(create "fields" Arg.(opt (some string) None doc))

let dns_server =
  let doc = Key.Arg.info ~doc:"DNS server IP." [ "dns-server" ] in
  Key.(create "dns-server" Arg.(required ip_address doc))

let dns_port =
  let doc = Key.Arg.info ~doc:"DNS server port." [ "dns-port" ] in
  Key.(create "dns-port" Arg.(opt int 53 doc))

let dns_key =
  let doc = Key.Arg.info ~doc:"nsupdate key (name:type:value,...)" [ "dns-key" ] in
  Key.(create "dns-key" Arg.(required string doc))

let selector =
  let doc = Key.Arg.info ~doc:"DKIM selector." [ "selector" ] in
  Key.(create "selector" Arg.(required string doc))

let domain =
  let doc = Key.Arg.info ~doc:"DKIM domain-name." [ "domain" ] in
  Key.(create "domain" Arg.(required string doc))

let destination =
  let doc = Key.Arg.info ~doc:"SMTP server destination." [ "destination" ] in
  Key.(create "destination" Arg.(required ipv4_address doc))

let timestamp =
  let doc = Key.Arg.info ~doc:"The epoch time that the private key was created." [ "timestamp" ] in
  Key.(create "timestamp" Arg.(opt (some int) None doc))

let expiration =
  let doc = Key.Arg.info ~doc:"The signature expiration (epoch time)." [ "expiration" ] in
  Key.(create "expiration" Arg.(opt (some int) None doc))

let private_key =
  let doc = Key.Arg.info ~doc:"The seed (in base64) of the private RSA key." [ "private-key" ] in
  Key.(create "private-key" Arg.(required string doc))

let postmaster =
  let doc = Key.Arg.info ~doc:"The postmaster of the SMTP service." [ "postmaster" ] in
  Key.(create "postmaster" Arg.(required string doc))

let certificate =
  let doc = Key.Arg.info ~doc:"The location of the TLS certificate." [ "certificate" ] in
  Key.(create "certificate" Arg.(required ~stage:`Both string doc))

let key_fingerprint =
  let doc = Key.Arg.info ~doc:"Authenticate TLS using public key fingerprint." [ "key-fingerprint" ] in
  Key.(create "key-fingerprint" Arg.(opt (some string) None doc))

let certificate_fingerprint =
  let doc = Key.Arg.info ~doc:"Authenticate TLS using certificate fingerprint." [ "cert-fingerprint" ] in
  Key.(create "cert-fingerprint" Arg.(opt (some string) None doc))

let cert_der =
  let doc = Key.Arg.info ~doc:"The certificate (DER x Base64)." [ "cert-der" ] in
  Key.(create "cert-der" Arg.(required string doc))

let cert_key =
  let doc = Key.Arg.info ~doc:"The private key of the certificate (seed in Base64)." [ "cert-key" ] in
  Key.(create "cert-key" Arg.(required string doc))

let pin_irmin = "git+https://github.com/mirage/irmin.git#ae15cc291ce4d6e77c130e1db41e3f92dae00e69"
let pin_git = "git+https://github.com/mirage/ocaml-git.git#42cd15baa8cb6e82f003f62126cf18f42cce8c63"
let pin_repr = "git+https://github.com/mirage/repr#0c0b7b76bd6531ce3d3adc341bf3df72046f5855"
let pin_dns = "git+https://github.com/mirage/ocaml-dns.git#d99eff3429f60a5a9eef262f45b4e7d12ab251e5"

let keys =
  Key.[ abstract fields
      ; abstract dns_server
      ; abstract dns_port
      ; abstract dns_key
      ; abstract selector
      ; abstract domain
      ; abstract destination
      ; abstract timestamp 
      ; abstract expiration
      ; abstract private_key
      ; abstract postmaster
      ; abstract key_fingerprint
      ; abstract certificate_fingerprint
      ; abstract cert_der
      ; abstract cert_key ]

let packages =
  [ package "randomconv"
  ; package "ptt" ~sublibs:[ "nec" ]
  ; package "ptt"
  ; package ~pin:pin_dns "dns"
  ; package ~pin:pin_dns "dns-client"
  ; package ~pin:pin_dns "dns-mirage"
  ; package ~pin:pin_dns "dns-tsig"
  ; package "domain-name"
  ; package ~pin:pin_dns "dns-mirage"
  ; package "ca-certs-nss" ]

let signer =
  foreign ~keys ~packages "Unikernel.Make" @@
  random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> job

let random = default_random
let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stack = generic_stackv4v6 default_network

let () =
  register "signer"
    [ signer $ random $ time $ mclock $ pclock $ stack ]
