open Mirage

let domain =
  let doc = Key.Arg.info ~doc:"SMTP domain-name." [ "domain" ] in
  Key.(create "domain" Arg.(required string doc))

let destination =
  let doc = Key.Arg.info ~doc:"SMTP server destination." [ "destination" ] in
  Key.(create "destination" Arg.(required ip_address doc))

let postmaster =
  let doc = Key.Arg.info ~doc:"The postmaster of the SMTP service." [ "postmaster" ] in
  Key.(create "postmaster" Arg.(required string doc))

let key_seed =
  let doc = Key.Arg.info ~doc:"Certificate key seed." [ "key-seed" ] in
  Key.(create "key-seed" Arg.(required string doc))

let dns_key =
  let doc = Key.Arg.info ~doc:"nsupdate key (name:type:value,...)" ["dns-key"] in
  Key.(create "dns-key" Arg.(required string doc))

let dns_server =
  let doc = Key.Arg.info ~doc:"IP of the primary DNS server." ["dns-server"] in
  Key.(create "dns-server" Arg.(required ip_address doc))

let dns_port =
  let doc = Key.Arg.info ~doc:"Port of the primary DNS server." ["dns-port"] in
  Key.(create "dns-port" Arg.(opt int 53 doc))

let nameservers =
  let doc = Key.Arg.info ~doc:"DNS nameserver used by the SPF verificator." [ "nameserver" ] in
  Key.(create "nameservers" Arg.(opt_all string doc))

let keys =
  Key.[ v domain
      ; v postmaster
      ; v destination
      ; v key_seed
      ; v dns_server
      ; v dns_port
      ; v dns_key ]

let packages =
  [ package "ptt" ~sublibs:[ "hm" ] ~pin:"git+https://github.com/mirage/ptt.git"
  ; package "mrmime" ~pin:"git+https://github.com/mirage/mrmime.git"
  ; package "spamtacus-bayesian" ~pin:"git+https://github.com/mirage/spamtacus.git#improve"
  ; package "spamtacus" ~pin:"git+https://github.com/mirage/spamtacus.git#improve"
  ; package "spamtacus-mirage" ~pin:"git+https://github.com/mirage/spamtacus.git#improve"
  ; package "uspf" ~pin:"git+https://github.com/dinosaure/uspf.git#d923cfae1e28a9d92e67b2bceeb24f2adf9086b8"
  ; package "dns"
  ; package "domain-name"
  ; package "ca-certs-nss"
  ; package "dns-mirage"
  ; package "dns-certify" ~sublibs:[ "mirage" ] ]

let verifier =
  foreign ~keys ~packages "Unikernel.Make" @@
  random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> dns_client @-> job

let random = default_random
let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stack = generic_stackv4v6 default_network
let dns = generic_dns_client ~nameservers stack

let () =
  register "verifier"
    [ verifier $ random $ time $ mclock $ pclock $ stack $ dns ]
