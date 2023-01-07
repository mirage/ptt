open Mirage

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(required string doc))

let ssh_key =
  let doc = Key.Arg.info ~doc:"The private SSH key." [ "ssh-key" ] in
  Key.(create "ssh_key" Arg.(opt (some string) None doc))

let ssh_authenticator =
  let doc = Key.Arg.info ~doc:"SSH public key of the remote Git repository." [ "ssh-authenticator" ] in
  Key.(create "ssh_authenticator" Arg.(opt (some string) None doc))

let domain =
  let doc = Key.Arg.info ~doc:"SMTP domain-name." [ "domain" ] in
  Key.(create "domain" Arg.(required string doc))

let postmaster =
  let doc = Key.Arg.info ~doc:"The postmaster of the SMTP service." [ "postmaster" ] in
  Key.(create "postmaster" Arg.(required string doc))

let nameservers =
  let doc = Key.Arg.info ~doc:"DNS nameserver used to resolve SMTP servers." [ "nameserver" ] in
  Key.(create "nameservers" Arg.(opt_all string doc))

let keys =
  Key.[ v domain
      ; v postmaster
      ; v remote ]

let packages =
  [ package "randomconv"
  ; package "ptt" ~sublibs:[ "value"; "mti-gf" ]
  ; package "git-kv"
  ; package "domain-name"
  ; package "dns-mirage" ]

let relay =
  foreign ~keys ~packages "Unikernel.Make" @@
  random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> dns_client @-> git_client @-> job

let random = default_random
let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stack = generic_stackv4v6 default_network
let dns = generic_dns_client ~nameservers stack
let tcp = tcpv4v6_of_stackv4v6 stack
let git_client =
  let happy_eyeballs = git_happy_eyeballs stack dns (generic_happy_eyeballs stack dns) in
  git_ssh ~key:ssh_key tcp happy_eyeballs

let () =
  register "relay"
    [ relay $ random $ time $ mclock $ pclock $ stack $ dns $ git_client ]
