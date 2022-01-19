open Rresult
open Lwt.Infix

let ( >>? ) = Lwt_result.bind

module Store = Irmin_unix.Git.Mem.KV (Ptt_irmin)
module Sync = Irmin.Sync (Store)

let ssh_edn, ssh_protocol = Mimic.register ~name:"ssh" (module SSH)

let make_context =
  Git_unix.ctx (Happy_eyeballs_lwt.create ()) >|= fun ctx ->
  let open Mimic in
  let k0 scheme user path host _port cap =
    match scheme with
    | `SSH -> Lwt.return_some (user, host, path, cap)
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

let local_to_string local =
  let pp ppf = function
    | `Atom x -> Fmt.string ppf x
    | `String v -> Fmt.pf ppf "%S" v in
  Fmt.str "%a" Fmt.(list ~sep:(any ".") pp) local

let add remote local password targets insecure =
  let tmp = R.failwith_error_msg (Bos.OS.Dir.tmp "git-%s") in
  let _ = R.failwith_error_msg (Bos.OS.Dir.create Fpath.(tmp / ".git")) in
  let _ =
    R.failwith_error_msg (Bos.OS.Dir.create Fpath.(tmp / ".git" / "refs")) in
  let _ =
    R.failwith_error_msg (Bos.OS.Dir.create Fpath.(tmp / ".git" / "tmp")) in
  let _ =
    R.failwith_error_msg (Bos.OS.Dir.create Fpath.(tmp / ".git" / "objects"))
  in
  let _ =
    R.failwith_error_msg
      (Bos.OS.Dir.create Fpath.(tmp / ".git" / "objects" / "pack")) in
  let config = Irmin_git.config (Fpath.to_string tmp) in
  Store.Repo.v config >>= Store.master >>= fun store ->
  make_context >>= fun ctx ->
  let remote = Store.remote ~ctx remote in
  Sync.pull store remote `Set >|= R.reword_error (fun err -> `Pull err)
  >>? fun _ ->
  let v = {Ptt_irmin.targets; password; insecure} in
  let info () =
    let date = Int64.of_float (Unix.gettimeofday ())
    and message = Fmt.str "New user %a added" Emile.pp_local local in
    Irmin.Info.v ~date ~author:"ptt.adduser" message in
  Store.set ~info store [local_to_string local] v
  >|= R.reword_error (fun err -> `Set err)
  >>? fun _ -> Sync.push store remote >|= R.reword_error (fun err -> `Push err)

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX socket -> Fmt.pf ppf "%s" socket
  | Unix.ADDR_INET (inet_addr, port) ->
    Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port

let run _ remote user pass targets insecure =
  match Lwt_main.run (add remote user pass targets insecure) with
  | Ok _ -> `Ok 0
  | Error (`Pull _err) -> `Error (false, Fmt.str "Unreachable Git repository.")
  | Error (`Set _err) ->
    `Error (false, Fmt.str "Unable to locally set the store.")
  | Error (`Push _err) ->
    `Error (false, Fmt.str "Unallowed to push to %s." remote)

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
  let env = Arg.env_var "PTT_LOGS" in
  Logs_cli.level ~docs:common_options ~env ()

let renderer =
  let env = Arg.env_var "PTT_FMT" in
  Fmt_cli.style_renderer ~docs:common_options ~env ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over () ; k () in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  {Logs.report}

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ()
  ; Logs.set_level level
  ; Logs.set_reporter (reporter Fmt.stderr)
  ; Option.is_none level

let setup_logs = Term.(const setup_logs $ renderer $ verbosity)

let cmd =
  let doc = "Add an user into the SMTP database." in
  let man = [] in
  ( Term.(
      ret
        (const run $ setup_logs $ remote $ user $ password $ targets $ insecure))
  , Term.info "useradd" ~doc ~man )

let () = Term.(exit_status @@ eval cmd)
