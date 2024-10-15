(* mirage >= 4.8.0 & < 4.9.0 *)

open Mirage

let setup = runtime_arg ~pos:__POS__ "Unikernel.K.setup"

let packages =
  [ package "randomconv"
  ; package "ptt" ~sublibs:[ "value"; "mti-gf" ]
  ; package "git-kv"
  ; package "domain-name"
  ; package "dns-mirage" ]

let runtime_args = [ setup ]

let relay =
  main ~runtime_args ~packages "Unikernel.Make" @@
  time @-> mclock @-> pclock @-> stackv4v6 @-> dns_client @-> happy_eyeballs @-> git_client @-> job

let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stack = generic_stackv4v6 default_network
let he = generic_happy_eyeballs stack
let dns = generic_dns_client stack he
let tcp = tcpv4v6_of_stackv4v6 stack
let git_client =
  let git = mimic_happy_eyeballs stack he dns in
  git_ssh tcp git

let () =
  register "relay"
    [ relay $ time $ mclock $ pclock $ stack $ dns $ he $ git_client ]
