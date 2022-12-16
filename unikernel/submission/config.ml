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

let destination =
  let doc = Key.Arg.info ~doc:"SMTP server destination." [ "destination" ] in
  Key.(create "destination" Arg.(required ip_address doc))

let domain =
  let doc = Key.Arg.info ~doc:"SMTP domain-name." [ "domain" ] in
  Key.(create "domain" Arg.(required string doc))

let postmaster =
  let doc = Key.Arg.info ~doc:"The postmaster of the SMTP service." [ "postmaster" ] in
  Key.(create "postmaster" Arg.(required string doc))

let key_seed =
  let doc = Key.Arg.info ~doc:"Certificate key seed." [ "key-seed" ] in
  Key.(create "key-seed" Arg.(required string doc))

let submission_domain =
  let doc = Key.Arg.info ~doc:"domain-name of the submission SMTP service." [ "submission-domain" ] in
  Key.(create "submission-domain" Arg.(required string doc))

let dns_key =
  let doc = Key.Arg.info ~doc:"nsupdate key (name:type:value,...)" ["dns-key"] in
  Key.(create "dns-key" Arg.(required string doc))

let dns_server =
  let doc = Key.Arg.info ~doc:"IP of the primary DNS server." ["dns-server"] in
  Key.(create "dns-server" Arg.(required ip_address doc))

let dns_port =
  let doc = Key.Arg.info ~doc:"Port of the primary DNS server." ["dns-port"] in
  Key.(create "dns-port" Arg.(opt int 53 doc))

let keys =
  Key.[ v domain
      ; v postmaster
      ; v remote
      ; v destination
      ; v submission_domain
      ; v key_seed
      ; v dns_server
      ; v dns_port
      ; v dns_key ]

let packages =
  [ package "randomconv"
  ; package "ptt" ~sublibs:[ "value"; "lipap" ] ~pin:"git+https://github.com/mirage/ptt.git"
  ; package "git-kv"
  ; package "dns-mirage"
  ; package "dns-certify" ~sublibs:[ "mirage" ]
  ; package "domain-name"
  ; package "ca-certs-nss"
  ; package "emile" ]

let submission =
  foreign ~keys ~packages "Unikernel.Make" @@
  random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> git_client @-> job

let random = default_random
let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stack = generic_stackv4v6 default_network
let dns = generic_dns_client stack
let tcp = tcpv4v6_of_stackv4v6 stack
let git_client =
  let happy_eyeballs = git_happy_eyeballs stack dns (generic_happy_eyeballs stack dns) in
  git_ssh ~key:ssh_key tcp happy_eyeballs

let () =
  register "submission"
    [ submission $ random $ time $ mclock $ pclock $ stack $ git_client ]
