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
  [ package "ptt" ~sublibs:[ "spartacus" ] ~pin:"git+https://github.com/mirage/ptt.git"
  ; package "mrmime" ~pin:"git+https://github.com/mirage/mrmime.git"
  ; package "spamtacus-bayesian" ~pin:"git+https://github.com/mirage/spamtacus.git#improve"
  ; package "spamtacus" ~pin:"git+https://github.com/mirage/spamtacus.git#improve"
  ; package "spamtacus-mirage" ~pin:"git+https://github.com/mirage/spamtacus.git#improve"
  ; package "colombe" ~pin:"git+https://github.com/mirage/colombe.git#96c2e9ba6cd04b87879e048110df2fa3f7f0644f"
  ; package "sendmail" ~pin:"git+https://github.com/mirage/colombe.git#96c2e9ba6cd04b87879e048110df2fa3f7f0644f"
  ; package "sendmail-lwt" ~pin:"git+https://github.com/mirage/colombe.git#96c2e9ba6cd04b87879e048110df2fa3f7f0644f"
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
