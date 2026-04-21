(* mirage >= 4.9.0 & < 4.11.0 *)

open Mirage

let setup = runtime_arg ~pos:__POS__ "Unikernel.K.setup"

let packages =
  [ package "randomconv"
  ; package "ptt" ~sublibs:[ "nec"; "fake-dns" ]
  ; package "uspf" ~pin:"git+https://github.com/mirage/uspf.git#a4b90d1e99a607b2d2e8af62f32d5b211787b45d"
  ; package "uspf-mirage" ~pin:"git+https://github.com/mirage/uspf.git#a4b90d1e99a607b2d2e8af62f32d5b211787b45d"
  ; package "dmarc" ~pin:"git+https://github.com/dinosaure/ocaml-dmarc.git#7789e3c0835f97c3a4e43e3c58d81a23880f2773"
  ; package "public-suffix" ~pin:"git+https://github.com/dinosaure/ocaml-dmarc.git#7789e3c0835f97c3a4e43e3c58d81a23880f2773"
  ; package "arc" ~pin:"git+https://git.robur.coop/robur/ocaml-arc.git#2ea6849a809b4abe4e7b37543e9459e1f7e9938a"
  ; package "dkim" ~pin:"git+https://github.com/mirage/ocaml-dkim.git#a497c0890ee077eecd691d0c87388d6c5404f66c"
  ; package "dkim-mirage" ~pin:"git+https://github.com/mirage/ocaml-dkim.git#a497c0890ee077eecd691d0c87388d6c5404f66c"
  ; package "colombe" ~pin:"git+https://github.com/mirage/colombe.git#fada7c8b2125e5b87e63f6e1d10a99443386aa9a"
  ; package "sendmail" ~pin:"git+https://github.com/mirage/colombe.git#fada7c8b2125e5b87e63f6e1d10a99443386aa9a"
  ; package "sendmail-mirage" ~pin:"git+https://github.com/mirage/colombe.git#fada7c8b2125e5b87e63f6e1d10a99443386aa9a"
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
