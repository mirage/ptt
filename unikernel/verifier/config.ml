(* mirage >= 4.8.0 & < 4.9.0 *)

open Mirage

let setup = runtime_arg ~pos:__POS__ "Unikernel.K.setup"

let packages =
  [ package "ptt" ~sublibs:[ "hm"; "fake-dns" ]
  ; package "uspf"
  ; package "dns"
  ; package "domain-name"
  ; package "ca-certs-nss"
  ; package "dns-mirage"
  ; package "dns-certify" ~sublibs:[ "mirage" ] ]

let runtime_args = [ setup ]

let verifier =
  main ~runtime_args ~packages "Unikernel.Make" @@
  random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> happy_eyeballs @-> job

let random = default_random
let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stack = generic_stackv4v6 default_network
let he = generic_happy_eyeballs stack

let () =
  register "verifier"
    [ verifier $ random $ time $ mclock $ pclock $ stack $ he ]
