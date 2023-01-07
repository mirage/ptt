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
  Key.(create "destination" Arg.(required ip_address doc))

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

let keys =
  Key.[ v fields
      ; v dns_server
      ; v dns_port
      ; v dns_key
      ; v selector
      ; v domain
      ; v destination
      ; v timestamp 
      ; v expiration
      ; v private_key
      ; v postmaster ]

let packages =
  [ package "randomconv"
  ; package "ptt" ~sublibs:[ "nec" ]
  ; package "dns"
  ; package "dns-client"
  ; package "dns-mirage"
  ; package "dns-tsig"
  ; package "domain-name"
  ; package "dns-mirage"
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
