open Common

let run () maildir_path host message =
  let maildir = Maildir.create ~pid:(Unix.getpid ()) ~host ~random maildir_path in
  let path = Maildir_unix.get maildir message in
  Fmt.pr "Load %a.\n%!" Fpath.pp path ; Ok ()

open Cmdliner

let host =
  let doc = "Hostname of machine." in
  Arg.(value & opt Conv.hostname (Unix.gethostname ()) & info [ "h"; "host" ] ~docv:"<host>" ~doc)

let message =
  let doc = "Message ID." in
  Arg.(required & pos ~rev:true 0 (some Conv.message) None & info [] ~docv:"<message>" ~doc)

let command =
  let doc = "Describe tool." in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Describe specified mail." ] in
  Term.(pure run $ Arguments.setup_fmt_and_logs $ Arguments.maildir_path $ host $ message),
  Term.info "describe" ~doc ~exits ~man
