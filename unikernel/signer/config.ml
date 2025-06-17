(* mirage >= 4.9.0 & < 4.10.0 *)

open Mirage

let setup = runtime_arg ~pos:__POS__ "Unikernel.K.setup"

let packages =
  [ package "randomconv"
  ; package "ptt" ~sublibs:[ "nec"; "fake-dns" ]
  ; package "uspf" ~pin:"git+https://github.com/mirage/uspf.git#a4b90d1e99a607b2d2e8af62f32d5b211787b45d"
  ; package "dmarc" ~pin:"git+https://github.com/dinosaure/ocaml-dmarc.git#7789e3c0835f97c3a4e43e3c58d81a23880f2773"
  ; package "arc" ~pin:"git+https://git.robur.coop/robur/ocaml-arc.git#a8c264e57bbebef60dd9fe2917675ff65c921534"
  ; package "dkim" ~pin:"git+https://github.com/mirage/ocaml-dkim.git#ef005fa0e887bee1340da9c0b25110910c6d9cb4"
  ; package "dkim-mirage" ~pin:"git+https://github.com/mirage/ocaml-dkim.git#ef005fa0e887bee1340da9c0b25110910c6d9cb4"
  ; package "dns"
  ; package "dns-client"
  ; package "dns-mirage"
  ; package "dns-tsig"
  ; package "domain-name"
  ; package "dns-mirage"
  ; package "ca-certs-nss" ]

let runtime_args = [ setup ]

let signer =
  main ~runtime_args ~packages "Unikernel.Make" @@
  stackv4v6 @-> happy_eyeballs @-> dns_client @-> job

let stack = generic_stackv4v6 default_network
let he = generic_happy_eyeballs stack
let dns = generic_dns_client stack he

let () =
  register "signer"
    [ signer $ stack $ he $ dns ]
