(* mirage >= 4.8.0 & < 4.11.0 *)

open Mirage

let setup = runtime_arg ~pos:__POS__ "Unikernel.K.setup"

let packages =
  [ package "ptt" ~sublibs:[ "spartacus"; "fake-dns" ]
  ; package "spamtacus-mirage"
  ; package "domain-name"
  ; package "ca-certs-nss" ]

let runtime_args = [ setup ]

let spamfilter =
  main ~runtime_args ~packages "Unikernel.Make" @@
  stackv4v6 @-> happy_eyeballs @-> job

let stack = generic_stackv4v6 default_network
let he = generic_happy_eyeballs stack

let () =
  register "spamfilter"
    [ spamfilter $ stack $ he ]
