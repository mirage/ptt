(* mirage >= 4.9.0 & < 4.10.0 *)

open Mirage

let setup = runtime_arg ~pos:__POS__ "Unikernel.K.setup"

let packages =
  [ package "ptt" ~sublibs:[ "elit"; "fake-dns" ]
  ; package "uspf"
  ; package "dns"
  ; package "data-encoding"
  ; package "domain-name"
  ; package "ca-certs-nss"
  ; package "dns-mirage"
  ; package "oneffs"
  ; package "dns-certify" ~sublibs:[ "mirage" ] ]

let runtime_args = [ setup ]

let elit =
  main ~runtime_args ~packages "Unikernel.Make" @@
  block @-> stackv4v6 @-> dns_client @-> happy_eyeballs @-> job

let stack = generic_stackv4v6 default_network
let he = generic_happy_eyeballs stack
let dns = generic_dns_client stack he
let block = block_of_file "database"

let () =
  register "elit"
    [ elit $ block $ stack $ dns $ he ]
