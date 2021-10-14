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

(* / *)

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(required string doc))

let ssh_seed =
  let doc = Key.Arg.info ~doc:"Seed of the private SSH key." [ "ssh-seed" ] in
  Key.(create "ssh_seed" Arg.(opt (some string) None doc))

let ssh_auth =
  let doc = Key.Arg.info ~doc:"SSH public key of the remote Git endpoint." [ "ssh-auth" ] in
  Key.(create "ssh_auth" Arg.(opt (some string) None doc))

let destination =
  let doc = Key.Arg.info ~doc:"SMTP server destination." [ "destination" ] in
  Key.(create "destination" Arg.(required ipv4_address doc))

let domain =
  let doc = Key.Arg.info ~doc:"SMTP domain-name." [ "domain" ] in
  Key.(create "domain" Arg.(required string doc))

let postmaster =
  let doc = Key.Arg.info ~doc:"The postmaster of the SMTP service." [ "postmaster" ] in
  Key.(create "postmaster" Arg.(required string doc))

let certificate =
  let doc = Key.Arg.info ~doc:"The location of the TLS certificate." [ "certificate" ] in
  Key.(create "certificate" Arg.(required ~stage:`Both string doc))

let key_fingerprint =
  let doc = Key.Arg.info ~doc:"Authenticate TLS using public key fingerprint." [ "key-fingerprint" ] in
  Key.(create "key-fingerprint" Arg.(opt (some string) None doc))

let certificate_fingerprint =
  let doc = Key.Arg.info ~doc:"Authenticate TLS using certificate fingerprint." [ "cert-fingerprint" ] in
  Key.(create "cert-fingerprint" Arg.(opt (some string) None doc))

let cert_der =
  let doc = Key.Arg.info ~doc:"The certificate (DER x Base64)." [ "cert-der" ] in
  Key.(create "cert-der" Arg.(opt (some string) None doc))

let cert_key =
  let doc = Key.Arg.info ~doc:"The private key of the certificate (seed in Base64)." [ "cert-key" ] in
  Key.(create "cert-key" Arg.(opt (some string) None doc))

let hostname =
  let doc = Key.Arg.info ~doc:"domain-name of the SMTP service." [ "hostname" ] in
  Key.(create "hostname" Arg.(opt (some string) None doc))

let production =
  let doc = Key.Arg.info ~doc:"Generate a production certificate." [ "production" ] in
  Key.(create "production" Arg.(opt bool false doc))

let email =
  let doc = Key.Arg.info ~doc:"Email associated to the let's encrypt certificate generation." [ "email" ] in
  Key.(create "email" Arg.(opt (some string) None doc))

let account_seed =
  let doc = Key.Arg.info ~doc:"Let's encrypt account seed." [ "account-seed" ] in
  Key.(create "account_seed" Arg.(opt (some string) None doc))

let cert_seed =
  let doc = Key.Arg.info ~doc:"Let's encrypt certificate seed." [ "cert-seed" ] in
  Key.(create "cert_seed" Arg.(opt (some string) None doc))

let keys =
  Key.[ abstract domain
      ; abstract postmaster
      ; abstract remote
      ; abstract destination
      ; abstract key_fingerprint
      ; abstract certificate_fingerprint
      ; abstract cert_der
      ; abstract cert_key
      ; abstract hostname
      ; abstract production
      ; abstract email
      ; abstract account_seed
      ; abstract cert_seed ]

let pin_irmin = "git+https://github.com/mirage/irmin.git#dc452f5bf314c4f1ac8bf1e5b56f386283fa3401"
let pin_git = "git+https://github.com/mirage/ocaml-git.git#efe1afffd9d391bce802ab52ebf30f80d9aa4716"
let pin_repr = "git+https://github.com/mirage/repr#17623decc3d499b83da1f5d71b298681100539a3"
let pin_dns = "git+https://github.com/mirage/ocaml-dns.git#1cc956368241beca6157eb4a3351174837e2bbd5"

let packages =
  [ package "randomconv"
  ; package "ptt" ~sublibs:[ "lipap" ]
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
  ; package ~pin:pin_dns ~sublibs:[ "mirage" ] "dns-client"
  ; package ~pin:pin_dns "dns-mirage"
  ; package ~pin:pin_dns "dns-tsig"
  ; package "paf" ~sublibs:[ "mirage" ]
  ; package "paf-le"
  ; package "domain-name"
  ; package "art"
  ; package "ca-certs-nss"
  ; package "emile"
  ; package ~pin:pin_dns "dns-mirage" ]

let submission =
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
  register "submission"
    [ submission $ random $ time $ mclock $ pclock $ stack $ mimic ]
