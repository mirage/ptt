open Mirage

(* NOTE(dinosaure): it's like a DNS client but it uses the primary DNS server to
   get the possible DKIM public key if it exists (like a client) or [nsupdate]
   the primary DNS server with what we got from the command-line. *)
let generic_dns_client timeout dns_server dns_port =
  let open Functoria.DSL in
  let pp_label name ppf = function
    | None -> ()
    | Some key -> Fmt.pf ppf "@ ~%s:%s" name key in
  let pp_opt name ppf = function
    | None -> ()
    | Some key -> Fmt.pf ppf "@ ?%s:%s" name key in
  let pop ~err x rest =
    match (rest, x) with
    | h :: t, Some _ -> (Some h, t)
    | _, None -> (None, rest)
    | _ -> err () in 
  let packages = [ package "dns-client-mirage" ~min:"9.0.0" ~max:"10.0.0" ] in
  let runtime_args = [ Runtime_arg.v dns_server; Runtime_arg.v dns_port; ] in
  let runtime_args = match timeout with
    | Some timeout -> runtime_args @ [ Runtime_arg.v timeout ]
    | None -> runtime_args in
  let pp_nameserver ppf (dns_server, dns_port) =
    let nameserver = Fmt.str "[\"tcp:%s:%s\"]" dns_server dns_port in
    pp_label "nameservers" ppf (Some nameserver)
  in
  let err () = connect_err "generic_dns_client" 6 ~max:9 in
  let connect _info modname = function
    | _random
      :: _time
      :: _mclock
      :: _pclock
      :: stackv4v6
      :: happy_eyeballs
      :: rest ->
        let[@warning "-8"] Some dns_server, rest = pop ~err (Some dns_server) rest in
        let[@warning "-8"] Some dns_port, rest = pop ~err (Some dns_port) rest in
        let timeout, rest = pop ~err timeout rest in
        let () = match rest with [] -> () | _ -> err () in
        code ~pos:__POS__ {ocaml|%s.connect @[%a%a@ (%s, %s)@]|ocaml} modname
          pp_nameserver (dns_server, dns_port) (pp_opt "timeout") timeout stackv4v6
          happy_eyeballs
    | _ -> err ()
  in
  impl ~runtime_args ~packages ~connect "Dns_client_mirage.Make"
    (random
    @-> time
    @-> mclock
    @-> pclock
    @-> stackv4v6
    @-> happy_eyeballs
    @-> dns_client)

let generic_dns_client ?timeout ?(random = default_random)
    ?(time = default_time) ?(mclock = default_monotonic_clock)
    ?(pclock = default_posix_clock) ~dns_server ~dns_port stackv4v6 happy_eyeballs =
  generic_dns_client timeout dns_server dns_port
  $ random
  $ time
  $ mclock
  $ pclock
  $ stackv4v6
  $ happy_eyeballs

let dns_server : Ipaddr.t Runtime_arg.arg = Runtime_arg.create ~pos:__POS__ "Unikernel.K.dns_server"
let dns_port : int Runtime_arg.arg = Runtime_arg.create ~pos:__POS__ "Unikernel.K.dns_port"
let setup = runtime_arg ~pos:__POS__ "Unikernel.K.setup"

let packages =
  [ package "randomconv"
  ; package "ptt" ~sublibs:[ "nec"; "fake-dns" ]
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
  random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> dns_client @-> happy_eyeballs @-> job

let random = default_random
let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stack = generic_stackv4v6 default_network
let he = generic_happy_eyeballs stack
let dns = generic_dns_client ~dns_server ~dns_port stack he

let () =
  register "signer"
    [ signer $ random $ time $ mclock $ pclock $ stack $ dns $ he ]
