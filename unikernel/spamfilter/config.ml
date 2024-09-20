open Mirage

let setup = runtime_arg ~pos:__POS__ "Unikernel.K.setup"

let packages =
  [ package "ptt" ~sublibs:[ "spartacus" ]
  ; package "spamtacus-mirage"
  ; package "domain-name"
  ; package "ca-certs-nss" ]

let runtime_args = [ setup ]

let spamfilter =
  main ~runtime_args ~packages "Unikernel.Make" @@
  time @-> mclock @-> pclock @-> stackv4v6 @-> job

let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stack = generic_stackv4v6 default_network

let () =
  register "spamfilter"
    [ spamfilter $ time $ mclock $ pclock $ stack ]
