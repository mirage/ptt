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

let keys =
  Key.[ v domain
      ; v destination
      ; v postmaster ]

let packages =
  [ package "ptt" ~sublibs:[ "spartacus" ]
  ; package "domain-name"
  ; package "ca-certs-nss" ]

let spamfilter =
  foreign ~keys ~packages "Unikernel.Make" @@
  random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> job

let random = default_random
let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stack = generic_stackv4v6 default_network

let () =
  register "spamfilter"
    [ spamfilter $ random $ time $ mclock $ pclock $ stack ]
