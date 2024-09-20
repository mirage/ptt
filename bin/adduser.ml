open Rresult
open Lwt.Infix

let ( >>? ) = Lwt_result.bind

module Store = Git_kv.Make (Pclock)

let local_to_string local =
  let pp ppf = function
    | `Atom x -> Fmt.string ppf x
    | `String v -> Fmt.pf ppf "%S" v in
  Fmt.str "%a" Fmt.(list ~sep:(any ".") pp) local |> Mirage_kv.Key.v

let ssh_edn, ssh_protocol = Mimic.register ~name:"ssh" (module SSH)

let unix_ctx_with_ssh () =
  Git_unix.ctx (Happy_eyeballs_lwt.create ()) >|= fun ctx ->
  let open Mimic in
  let k0 scheme user path host port mode =
    match scheme, Unix.gethostbyname host with
    | `SSH, {Unix.h_addr_list; _} when Array.length h_addr_list > 0 ->
      Lwt.return_some {SSH.user; path; host= h_addr_list.(0); port; mode}
    | _ -> Lwt.return_none in
  ctx
  |> Mimic.fold Smart_git.git_transmission
       Fun.[req Smart_git.git_scheme]
       ~k:(function `SSH -> Lwt.return_some `Exec | _ -> Lwt.return_none)
  |> Mimic.fold ssh_edn
       Fun.
         [
           req Smart_git.git_scheme; req Smart_git.git_ssh_user
         ; req Smart_git.git_path; req Smart_git.git_hostname
         ; dft Smart_git.git_port 22; req Smart_git.git_capabilities
         ]
       ~k:k0

let add remote local password targets insecure : (unit, _) result Lwt.t =
  unix_ctx_with_ssh () >>= fun ctx ->
  Git_kv.connect ctx remote >>= fun store ->
  let v = {Ptt_value.targets; password; insecure} in
  let v = Ptt_value.to_string_json v in
  ( Store.change_and_push store @@ fun store ->
    Store.set store (local_to_string local) v )
  >|= Result.join
  >>= function
  | Ok _ as v -> Lwt.return v
  | Error err -> Lwt.return_error (R.msgf "%a" Store.pp_write_error err)

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX socket -> Fmt.pf ppf "%s" socket
  | Unix.ADDR_INET (inet_addr, port) ->
    Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port

let run _ remote user pass targets insecure =
  match Lwt_main.run (add remote user pass targets insecure) with
  | Ok () -> `Ok 0
  | Error (`Msg err) -> `Error (false, err)

open Cmdliner

let remote =
  let parser str =
    match Smart_git.Endpoint.of_string str with
    | Ok _ -> Ok str
    | Error _ as err -> err in
  Arg.conv (parser, Fmt.string)

let user =
  let parser str =
    match Angstrom.parse_string ~consume:All Emile.Parser.local_part str with
    | Ok _ as v -> v
    | Error _ -> R.error_msgf "Invalid username: %S" str in
  let pp ppf local =
    let pp ppf = function
      | `Atom x -> Fmt.string ppf x
      | `String v -> Fmt.pf ppf "%S" v in
    Fmt.(list ~sep:(any ".") pp) ppf local in
  Arg.conv (parser, pp)

let password =
  let parser str = Ok (Digestif.BLAKE2B.digest_string str) in
  Arg.conv (parser, Digestif.BLAKE2B.pp)

let target =
  let parser str =
    match Emile.of_string str with
    | Ok _ as v -> v
    | Error _ -> R.error_msgf "Invalid email address: %S" str in
  Arg.conv (parser, Emile.pp_mailbox)

let remote =
  let doc = "The Git repository." in
  Arg.(required & opt (some remote) None & info ["r"; "remote"] ~doc)

let user =
  let doc = "The username." in
  Arg.(required & pos 0 (some user) None & info [] ~doc ~docv:"<username>")

let password =
  let doc = "The password." in
  Arg.(required & pos 1 (some password) None & info [] ~doc ~docv:"<password>")

let targets =
  let doc = "Targets of the email." in
  Arg.(value & opt (list ~sep:',' target) [] & info ["t"; "targets"] ~doc)

let insecure =
  let doc = "Allow to send an email without STARTTLS." in
  Arg.(value & flag & info ["insecure"] ~doc)

let common_options = "COMMON OPTIONS"

let verbosity =
  let env = Cmd.Env.info "PTT_LOGS" in
  Logs_cli.level ~docs:common_options ~env ()

let renderer =
  let env = Cmd.Env.info "PTT_FMT" in
  Fmt_cli.style_renderer ~docs:common_options ~env ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  {Logs.report}

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter Fmt.stderr);
  Option.is_none level

let setup_logs = Term.(const setup_logs $ renderer $ verbosity)

let term =
  Term.(
    ret (const run $ setup_logs $ remote $ user $ password $ targets $ insecure))

let cmd =
  let doc = "Add an user into the SMTP database." in
  let man = [] in
  Cmd.v (Cmd.info "useradd" ~doc ~man) term

let () = exit (Cmd.eval' cmd)
