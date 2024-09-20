open Mirage

let ssh_key =
  Runtime_arg.create ~pos:__POS__
    {|let open Cmdliner in
      let doc = Arg.info ~doc:"The private SSH key (rsa:<seed> or ed25519:<b64-key>)." ["ssh-key"] in
      Arg.(value & opt (some string) None doc)|}

let ssh_authenticator =
  Runtime_arg.create ~pos:__POS__
    {|let open Cmdliner in
      let doc = Arg.info ~doc:"SSH authenticator." ["ssh-auth"] in
      Arg.(value & opt (some string) None doc)|}

let ssh_password =
  Runtime_arg.create ~pos:__POS__
    {|let open Cmdliner in
      let doc = Arg.info ~doc:"The private SSH password." [ "ssh-password" ] in
      Arg.(value & opt (some string) None doc)|}

let nameservers = Runtime_arg.create ~pos:__POS__ "Unikernel.K.nameservers"
let setup = runtime_arg ~pos:__POS__ "Unikernel.K.setup"

let packages =
  [ package "randomconv"
  ; package "ptt" ~sublibs:[ "value"; "lipap" ]
  ; package "git-kv"
  ; package "dns-mirage"
  ; package "dns-certify" ~sublibs:[ "mirage" ]
  ; package "domain-name"
  ; package "ca-certs-nss"
  ; package "emile" ]

let runtime_args = [ setup ]

let submission =
  main ~runtime_args ~packages "Unikernel.Make" @@
  random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> git_client @-> job

let random = default_random
let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stack = generic_stackv4v6 default_network
let he = generic_happy_eyeballs stack
let dns = generic_dns_client ~nameservers stack he
let tcp = tcpv4v6_of_stackv4v6 stack
let git_client =
  let git = mimic_happy_eyeballs stack he dns in
  git_ssh ~password:ssh_password ~key:ssh_key ~authenticator:ssh_authenticator tcp git

let () =
  register "submission"
    [ submission $ random $ time $ mclock $ pclock $ stack $ git_client ]
