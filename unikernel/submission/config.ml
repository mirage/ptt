(* mirage >= 4.8.0 & < 4.9.0 *)

open Mirage

let setup = runtime_arg ~pos:__POS__ "Unikernel.K.setup"

let packages =
  [ package "randomconv"
  ; package "ptt" ~sublibs:[ "value"; "lipap"; "fake-dns" ]
  ; package "git-kv"
  ; package "dns-mirage"
  ; package "dns-certify" ~sublibs:[ "mirage" ]
  ; package "domain-name"
  ; package "ca-certs-nss"
  ; package "emile" ]

let runtime_args = [ setup ]

let submission =
  main ~runtime_args ~packages "Unikernel.Make" @@
  random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> happy_eyeballs @-> git_client @-> job

let random = default_random
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
  register "submission"
    [ submission $ random $ time $ mclock $ pclock $ stack $ he $ git_client ]
