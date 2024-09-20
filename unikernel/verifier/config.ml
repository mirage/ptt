open Mirage

let nameservers = Runtime_arg.create ~pos:__POS__ "Unikernel.K.nameservers"
let setup = runtime_arg ~pos:__POS__ "Unikernel.K.setup"

let packages =
  [ package "ptt" ~sublibs:[ "hm" ]
  ; package "uspf"
  ; package "dns"
  ; package "domain-name"
  ; package "ca-certs-nss"
  ; package "dns-mirage"
  ; package "dns-certify" ~sublibs:[ "mirage" ] ]

let runtime_args = [ setup ]

let verifier =
  main ~runtime_args ~packages "Unikernel.Make" @@
  random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> dns_client @-> job

let random = default_random
let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stack = generic_stackv4v6 default_network
let he = generic_happy_eyeballs stack
let dns = generic_dns_client ~nameservers stack he

let () =
  register "verifier"
    [ verifier $ random $ time $ mclock $ pclock $ stack $ dns ]
