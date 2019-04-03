open Common

let src = Logs.Src.create "mti-gf.verify" ~doc:"logs mti-gf's verify event"
module Log = (val Logs.src_log src : Logs.LOG)

let parse_in_channel new_line ic =
  let open Angstrom.Buffered in
  let raw = Bytes.create 4096 in
  let rec go = function
    | Partial continue ->
      let len = input ic raw 0 (Bytes.length raw) in
      let raw = sanitize_input new_line raw len in
      let res = if len = 0 then continue `Eof else continue (`String raw) in
      (go[@tailcall]) res
    | Done (_, _) -> close_in ic ; Ok ()
    | Fail (_, _, err) -> close_in ic ; Error (`Msg err) in
  go (parse Mrmime.Mail.mail)

let just_verify maildir new_line message =
  let open Rresult.R in
  Fmt.pr "%a %a.%!" Pretty_printer.pp_wait () Fmt.(using Maildir.value Maildir.pp_message) message ;
  in_channel_of_message maildir message >>= fun ic ->
  parse_in_channel new_line ic |> function
  | Ok _ as v ->
    Fmt.pr "\r%a %a.\n%!" Pretty_printer.pp_ok () Fmt.(using Maildir.value Maildir.pp_message) message ; v
  | Error (`Msg err) ->
    Fmt.pr "\r%a %a: %s.\n%!" Pretty_printer.pp_error () Fmt.(using Maildir.value Maildir.pp_message) message err ;
    error_msgf "On %a: %s" Fmt.(using Maildir.value Maildir.pp_message) message err

let process verify_only_new_messages maildir new_line acc message =
  if Maildir.is_new message || not verify_only_new_messages
  then
    match just_verify maildir new_line message with
    | Error (`Msg err) ->
      Log.err (fun m -> m "Retrieve an error: %s." err) ;
      false && acc
    | Ok () -> true && acc
  else acc

let run () maildir_path host verify_only_new_messages new_line =
  let maildir = Maildir.create ~pid:(Unix.getpid ()) ~host ~random maildir_path in

  let res = Maildir_unix.(fold (process verify_only_new_messages maildir new_line) true fs maildir) in
  if res then Ok () else Rresult.R.error (`Msg "Retrieve an invalid e-mail")

open Cmdliner

let host =
  let doc = "Hostname of machine." in
  Arg.(value & opt Conv.hostname (Unix.gethostname ()) & info [ "h"; "host" ] ~docv:"<host>" ~doc)

let verify =
  let doc = "Verify only new messages." in
  Arg.(value & flag & info [ "only-new" ] ~doc)

let command =
  let doc = "Verify tool." in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Verify mails from a <maildir> directory." ] in
  Term.(pure run
        $ Argument.setup_fmt_and_logs
        $ Argument.maildir_path
        $ host
        $ verify
        $ Argument.new_line),
  Term.info "verify" ~doc ~exits ~man
