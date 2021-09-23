open Mirage

type mimic = Mimic

let mimic = typ Mimic

let mimic_count =
  let v = ref (-1) in
  fun () -> incr v ; !v

let mimic_conf () =
  let packages = [ package "mimic" ] in
  impl @@ object
       inherit base_configurable
       method ty = mimic @-> mimic @-> mimic
       method module_name = "Mimic.Merge"
       method! packages = Key.pure packages
       method name = Fmt.str "merge_ctx%02d" (mimic_count ())
       method! connect _ _modname =
         function
         | [ a; b ] -> Fmt.str "Lwt.return (Mimic.merge %s %s)" a b
         | [ x ] -> Fmt.str "%s.ctx" x
         | _ -> Fmt.str "Lwt.return Mimic.empty"
     end

let merge ctx0 ctx1 = mimic_conf () $ ctx0 $ ctx1

let mimic_tcp_conf =
  let packages = [ package "git-mirage" ~sublibs:[ "tcp" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4v6 @-> mimic
       method module_name = "Git_mirage_tcp.Make"
       method! packages = Key.pure packages
       method name = "tcp_ctx"
       method! connect _ modname = function
         | [ stack ] ->
           Fmt.str {ocaml|Lwt.return (%s.with_stack %s %s.ctx)|ocaml}
             modname stack modname
         | _ -> assert false
     end

let mimic_tcp_impl stackv4v6 = mimic_tcp_conf $ stackv4v6

let mimic_ssh_conf ~kind ~seed ~auth =
  let seed = Key.abstract seed in
  let auth = Key.abstract auth in
  let packages = [ package "git-mirage" ~sublibs:[ "ssh" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4v6 @-> mimic @-> mclock @-> mimic
       method! keys = [ seed; auth; ]
       method module_name = "Git_mirage_ssh.Make"
       method! packages = Key.pure packages
       method name = match kind with
         | `Rsa -> "ssh_rsa_ctx"
         | `Ed25519 -> "ssh_ed25519_ctx"
       method! connect _ modname =
         function
         | [ _; tcp_ctx; _ ] ->
             let with_key =
               match kind with
               | `Rsa -> "with_rsa_key"
               | `Ed25519 -> "with_ed25519_key"
             in
             Fmt.str
               {ocaml|let ssh_ctx00 = Mimic.merge %s %s.ctx in
                      let ssh_ctx01 = Option.fold ~none:ssh_ctx00 ~some:(fun v -> %s.%s v ssh_ctx00) %a in
                      let ssh_ctx02 = Option.fold ~none:ssh_ctx01 ~some:(fun v -> %s.with_authenticator v ssh_ctx01) %a in
                      Lwt.return ssh_ctx02|ocaml}
               tcp_ctx modname
               modname with_key Key.serialize_call seed
               modname Key.serialize_call auth
         | _ -> assert false
     end

let mimic_ssh_impl ~kind ~seed ~auth stackv4v6 mimic_git mclock =
  mimic_ssh_conf ~kind ~seed ~auth
  $ stackv4v6
  $ mimic_git
  $ mclock

(* TODO(dinosaure): user-defined nameserver and port. *)

let mimic_dns_conf =
  let packages = [ package "git-mirage" ~sublibs:[ "dns" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = random @-> mclock @-> time @-> stackv4v6 @-> mimic @-> mimic
       method module_name = "Git_mirage_dns.Make"
       method! packages = Key.pure packages
       method name = "dns_ctx"
       method! connect _ modname =
         function
         | [ _; _; _; stack; tcp_ctx ] ->
             Fmt.str
               {ocaml|let dns_ctx00 = Mimic.merge %s %s.ctx in
                      let dns_ctx01 = %s.with_dns %s dns_ctx00 in
                      Lwt.return dns_ctx01|ocaml}
               tcp_ctx modname
               modname stack
         | _ -> assert false
     end

let mimic_dns_impl random mclock time stackv4v6 mimic_tcp =
  mimic_dns_conf $ random $ mclock $ time $ stackv4v6 $ mimic_tcp

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(required string doc))

let ssh_seed =
  let doc = Key.Arg.info ~doc:"Seed of the private SSH key." [ "ssh-seed" ] in
  Key.(create "ssh_seed" Arg.(opt (some string) None doc))

let ssh_auth =
  let doc = Key.Arg.info ~doc:"SSH public key of the remote Git endpoint." [ "ssh-auth" ] in
  Key.(create "ssh_auth" Arg.(opt (some string) None doc))

let domain =
  let doc = Key.Arg.info ~doc:"SMTP domain-name." [ "domain" ] in
  Key.(create "domain" Arg.(required string doc))

let postmaster =
  let doc = Key.Arg.info ~doc:"The postmaster of the SMTP service." [ "postmaster" ] in
  Key.(create "postmaster" Arg.(required string doc))

let cert_der =
  let doc = Key.Arg.info ~doc:"The certificate (DER x Base64)." [ "cert-der" ] in
  Key.(create "cert-der" Arg.(required string doc))

let cert_key =
  let doc = Key.Arg.info ~doc:"The private key of the certificate (seed in Base64)." [ "cert-key" ] in
  Key.(create "cert-key" Arg.(required string doc))

let dns_resolver =
  let doc = Key.Arg.info ~doc:"The DNS resolver." [ "resolver" ] in
  Key.(create "resolver" Arg.(opt (some string) None doc))

let keys =
  Key.[ abstract domain
      ; abstract postmaster
      ; abstract remote
      ; abstract dns_resolver
      ; abstract cert_der
      ; abstract cert_key ]

let pin_irmin = "git+https://github.com/mirage/irmin.git#ae15cc291ce4d6e77c130e1db41e3f92dae00e69"
let pin_git = "git+https://github.com/mirage/ocaml-git.git#42cd15baa8cb6e82f003f62126cf18f42cce8c63"
let pin_repr = "git+https://github.com/mirage/repr#0c0b7b76bd6531ce3d3adc341bf3df72046f5855"
let pin_dns = "git+https://github.com/mirage/ocaml-dns.git#eb8bac066cdc97e1a12bb1ccda854dd539092cf1"

let packages =
  [ package "randomconv"
  ; package "ptt" ~sublibs:[ "mti-gf" ]
  ; package "ptt" ~sublibs:[ "irmin" ]
  ; package ~pin:pin_irmin "irmin-mirage-git"
  ; package ~pin:pin_irmin "irmin-mirage"
  ; package ~pin:pin_git "git-mirage"
  ; package ~pin:pin_git "git-paf"
  ; package ~pin:pin_git "git-unix"
  ; package ~pin:pin_git "git-cohttp"
  ; package ~pin:pin_git "git-cohttp-unix"
  ; package ~pin:pin_git "git"
  ; package "ptt"
  ; package ~pin:pin_irmin "irmin"
  ; package ~pin:pin_irmin "irmin-git"
  ; package ~pin:pin_irmin "ppx_irmin"
  ; package ~pin:pin_repr "repr"
  ; package ~pin:pin_repr "ppx_repr"
  ; package ~pin:pin_dns "dns"
  ; package ~pin:pin_dns "dns-client"
  ; package ~pin:pin_dns "dns-mirage"
  ; package ~pin:pin_dns "dns-tsig"
  ; package "domain-name"
  ; package ~pin:pin_dns "dns-mirage" ]

let relay =
  foreign ~keys ~packages "Unikernel.Make" @@
  random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> mimic @-> job

let mimic ~kind ~seed ~auth stackv4v6 random mclock time =
  let mtcp = mimic_tcp_impl stackv4v6 in
  let mdns = mimic_dns_impl random mclock time stackv4v6 mtcp in
  let mssh = mimic_ssh_impl ~kind ~seed ~auth stackv4v6 mtcp mclock in
  merge mssh mdns

let random = default_random
let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stack = generic_stackv4v6 default_network
let mimic = mimic ~kind:`Rsa ~seed:ssh_seed ~auth:ssh_auth stack random mclock time

let () =
  register "relay"
    [ relay $ random $ time $ mclock $ pclock $ stack $ mimic ]
