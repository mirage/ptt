open Mirage

let domain =
  let doc = Key.Arg.info ~doc:"SMTP domain-name." [ "domain" ] in
  Key.(create "domain" Arg.(required string doc))

let destination =
  let doc = Key.Arg.info ~doc:"SMTP server destination." [ "destination" ] in
  Key.(create "destination" Arg.(required ipv4_address doc))

let postmaster =
  let doc = Key.Arg.info ~doc:"The postmaster of the SMTP service." [ "postmaster" ] in
  Key.(create "postmaster" Arg.(required string doc))

let key_fingerprint =
  let doc = Key.Arg.info ~doc:"Authenticate TLS using public key fingerprint." [ "key-fingerprint" ] in
  Key.(create "key-fingerprint" Arg.(opt (some string) None doc))

let certificate_fingerprint =
  let doc = Key.Arg.info ~doc:"Authenticate TLS using certificate fingerprint." [ "cert-fingerprint" ] in
  Key.(create "cert-fingerprint" Arg.(opt (some string) None doc))

let cert_der =
  let doc = Key.Arg.info ~doc:"The certificate (DER x Base64)." [ "cert-der" ] in
  Key.(create "cert-der" Arg.(opt (some string) None doc))

let cert_key =
  let doc = Key.Arg.info ~doc:"The private key of the certificate (seed in Base64)." [ "cert-key" ] in
  Key.(create "cert-key" Arg.(opt (some string) None doc))

let key_seed =
  let doc = Key.Arg.info ~doc:"Private key seed" [ "key-seed" ] in
  Key.(create "key-seed" Arg.(opt (some string) None doc))

let hostname =
  let doc = Key.Arg.info ~doc:"domain-name of the SMTP service." [ "hostname" ] in
  Key.(create "hostname" Arg.(opt (some string) None doc))

let dns_key =
  let doc = Key.Arg.info ~doc:"nsupdate key (name:type:value,...)" ["dns-key"] in
  Key.(create "dns-key" Arg.(required string doc))

let dns_server =
  let doc = Key.Arg.info ~doc:"DNS server IP" ["dns-server"] in
  Key.(create "dns-server" Arg.(required ip_address doc))

let dns_port =
  let doc = Key.Arg.info ~doc:"DNS server port" ["dns-port"] in
  Key.(create "dns-port" Arg.(opt int 53 doc))

let dns_resolver =
  let doc = Key.Arg.info ~doc:"The DNS resolver." [ "resolver" ] in
  Key.(create "resolver" Arg.(opt (some string) None doc))

let keys =
  Key.[ abstract domain
      ; abstract postmaster
      ; abstract destination
      ; abstract key_fingerprint
      ; abstract certificate_fingerprint
      ; abstract cert_der
      ; abstract cert_key
      ; abstract hostname
      ; abstract key_seed
      ; abstract dns_server
      ; abstract dns_port
      ; abstract dns_key
      ; abstract dns_resolver ]

let packages =
  [ package "ptt" ~sublibs:[ "hm" ]
  ; package "ptt"
  ; package "uri"
  ; package "dns"
  ; package "domain-name"
  ; package "ca-certs-nss"
  ; package "dns-mirage"
  ; package "dns-certify" ~sublibs:[ "mirage" ] ]

let verifier =
  foreign ~keys ~packages "Unikernel.Make" @@
  random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> job

let random = default_random
let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stack = generic_stackv4v6 default_network

let () =
  register "verifier"
    [ verifier $ random $ time $ mclock $ pclock $ stack ]
