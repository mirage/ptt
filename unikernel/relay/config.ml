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

(* TODO(dinosaure): [timeout] and [timer interval]. *)
let mimic_happy_eyeballs =
  let packages = [ package "git-mirage" ~sublibs:[ "happy-eyeballs" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> mimic
       method module_name = "Git_mirage_happy_eyeballs.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_happy_eyeballs"
       method! connect _ modname = function
         | [ _random; _time; _mclock; _pclock; stackv4v6; ] ->
           Fmt.str {ocaml|%s.connect %s|ocaml} modname stackv4v6
         | _ -> assert false
     end

let mimic_tcp =
  let packages = [ package "git-mirage" ~sublibs:[ "tcp" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = tcpv4v6 @-> mimic @-> mimic
       method module_name = "Git_mirage_tcp.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_tcp"
       method! connect _ modname = function
         | [ _tcpv4v6; ctx ] ->
           Fmt.str {ocaml|%s.connect %s|ocaml}
             modname ctx
         | _ -> assert false
     end

let mimic_ssh ?authenticator key =
  let packages = [ package "git-mirage" ~sublibs:[ "ssh" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = mclock @-> tcpv4v6 @-> mimic @-> mimic
       method! keys = match authenticator with
         | Some authenticator -> [ Key.abstract key; Key.abstract authenticator ]
         | None -> [ Key.abstract key ]
       method module_name = "Git_mirage_ssh.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_ssh"
       method! connect _ modname = function
         | [ _mclock; _tcpv4v6; ctx ] ->
           ( match authenticator with
           | None ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optionnal_key ~key:%a|ocaml}
               modname ctx modname Key.serialize_call (Key.abstract key)
           | Some authenticator ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optionnal_key ?authenticator:%a ~key:%a|ocaml}
               modname ctx modname
               Key.serialize_call (Key.abstract authenticator)
               Key.serialize_call (Key.abstract key) )
         | _ -> assert false
     end

let mimic_http ?tls_key_fingerprint ?tls_cert_fingerprint headers =
  let packages = [ package "git-mirage" ~sublibs:[ "http" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = time @-> pclock @-> tcpv4v6 @-> mimic @-> mimic
       method! keys = match tls_key_fingerprint, tls_cert_fingerprint with
         | Some tls_key_fingerprint, None ->
           let keys = match headers with Some headers -> [ Key.abstract headers ] | None -> [] in
           [ Key.abstract tls_key_fingerprint ] @ keys
         | None, Some tls_cert_fingerprint ->
           let keys = match headers with Some headers -> [ Key.abstract headers ] | None -> [] in
           [ Key.abstract tls_cert_fingerprint ] @ keys
         | Some tls_key_fingerprint, Some tls_cert_fingerprint ->
           let keys = match headers with Some headers -> [ Key.abstract headers ] | None -> [] in
           [ Key.abstract tls_key_fingerprint; Key.abstract tls_cert_fingerprint ] @ keys
         | None, None -> ( match headers with Some headers -> [ Key.abstract headers ] | None -> [] )
       method module_name = "Git_mirage_http.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_http"
       method! connect _ modname = function
         | [ _time; _pclock; _tcpv4v6; ctx; ] ->
           let serialize_headers ppf = function
             | None -> ()
             | Some headers -> Fmt.pf ppf "?headers:%a" Key.serialize_call (Key.abstract headers) in
           ( match tls_key_fingerprint, tls_cert_fingerprint with
           | Some tls_key_fingerprint, None ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers ?tls_key_fingerprint:%a%a|ocaml}
               modname ctx modname
               Key.serialize_call (Key.abstract tls_key_fingerprint)
               Fmt.((const string " ") ++ serialize_headers) headers
           | None, Some tls_cert_fingerprint ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers ?tls_cert_fingerprint:%a%a|ocaml}
               modname ctx modname
               Key.serialize_call (Key.abstract tls_cert_fingerprint)
               Fmt.((const string " ") ++ serialize_headers) headers
           | None, None ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers%a|ocaml}
               modname ctx modname
               Fmt.((const string " ") ++ serialize_headers) headers
           | Some tls_key_fingerprint, Some tls_cert_fingerprint ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers
                              ?tls_key_fingerprint:%a ?tls_cert_fingerprint:%a%a|ocaml}
               modname ctx modname
               Key.serialize_call (Key.abstract tls_key_fingerprint)
               Key.serialize_call (Key.abstract tls_cert_fingerprint)
               Fmt.((const string " ") ++ serialize_headers) headers )
         | _ -> assert false
     end

let tcpv4v6_of_stackv4v6 =
  impl @@ object
       inherit base_configurable
       method ty = stackv4v6 @-> tcpv4v6
       method module_name = "Git_mirage_happy_eyeballs.TCPV4V6"
       method! packages = Key.pure [ package "git-mirage" ~sublibs:[ "happy-eyeballs" ] ]
       method name = "tcpv4v6"
       method! connect _ modname = function
         | [ stackv4v6 ] -> Fmt.str {ocaml|%s.connect %s|ocaml} modname stackv4v6
         | _ -> assert false
     end

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(required string doc))

let ssh_key =
  let doc = Key.Arg.info ~doc:"The private SSH key." [ "ssh-key" ] in
  Key.(create "ssh_seed" Arg.(opt (some string) None doc))

let ssh_authenticator =
  let doc = Key.Arg.info ~doc:"SSH public key of the remote Git repository." [ "ssh-authenticator" ] in
  Key.(create "ssh_authenticator" Arg.(opt (some string) None doc))

let tls_key_fingerprint =
  let doc = Key.Arg.info ~doc:"The fingerprint of the TLS key." [ "tls-key-fingerprint" ] in
  Key.(create "tls_key_fingerprint" Arg.(opt (some string) None doc))

let tls_cert_fingerprint =
  let doc = Key.Arg.info ~doc:"The fingerprint of the TLS certificate." [ "tls-cert-fingerprint" ] in
  Key.(create "tls_cert_fingerprint" Arg.(opt (some string) None doc))

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

let pin_irmin = "git+https://github.com/dinosaure/irmin.git#65d5fc6ef7d6dd76d103559edee0ad6036a896af"

let packages =
  [ package "randomconv"
  ; package "ptt" ~sublibs:[ "mti-gf" ]
  ; package "ptt" ~sublibs:[ "irmin" ]
  ; package ~pin:pin_irmin "irmin-mirage-git"
  ; package ~pin:pin_irmin "irmin-mirage"
  ; package ~pin:pin_irmin "irmin-git"
  ; package ~pin:pin_irmin "irmin-unix"
  ; package ~pin:pin_irmin "irmin-pack"
  ; package ~pin:pin_irmin "irmin-http"
  ; package ~pin:pin_irmin "irmin-tezos"
  ; package ~pin:pin_irmin "irmin-graphql"
  ; package ~pin:pin_irmin "irmin-layers"
  ; package ~pin:pin_irmin "irmin-fs"
  ; package ~pin:pin_irmin "irmin"
  ; package ~pin:pin_irmin "ppx_irmin"
  ; package "ptt"
  ; package "domain-name"
  ; package "dns-mirage" ]

let relay =
  foreign ~keys ~packages "Unikernel.Make" @@
  random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> mimic @-> job

let mimic random stackv4v6 mclock pclock time =
  let tcpv4v6 = tcpv4v6_of_stackv4v6 $ stackv4v6 in
  let mhappy_eyeballs = mimic_happy_eyeballs $ random $ time $ mclock $ pclock $ stackv4v6 in
  let mtcp  = mimic_tcp
    $ tcpv4v6 $ mhappy_eyeballs in
  let mssh  = mimic_ssh ~authenticator:ssh_authenticator ssh_key
    $ mclock $ tcpv4v6 $ mhappy_eyeballs in
  let mhttp = mimic_http ~tls_key_fingerprint ~tls_cert_fingerprint None
    $ time $ pclock $ tcpv4v6 $ mhappy_eyeballs in
  merge mhttp (merge mtcp mssh)

let random = default_random
let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stack = generic_stackv4v6 default_network
let mimic = mimic random stack mclock pclock time

let () =
  register "relay"
    [ relay $ random $ time $ mclock $ pclock $ stack $ mimic ]
