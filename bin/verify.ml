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

let verify_dkim maildir new_line message =
  let open Rresult.R in
  match in_channel_of_message maildir message
    >>= fun ic -> let res = Ptt_dkim.verify ic new_line in close_in ic ; res with
  | Error (`Msg err) ->
    Fmt.pr "- dkim error: %s.\n%!" err
  | Ok lst ->
    List.iter
      (fun (domain, verified) -> match verified with
         | true -> Fmt.pr "- dkim on %a ok.\n%!" Domain_name.pp domain
         | false -> Fmt.pr "- dkim on %a error.\n%!" Domain_name.pp domain)
      lst

let just_verify maildir dkim new_line message =
  let open Rresult.R in
  Fmt.pr "%a.\n%!" Fmt.(using Maildir.value Maildir.pp_message) message ;
  in_channel_of_message maildir message >>= fun ic ->
  parse_in_channel new_line ic |> function
  | Ok _ as v ->
    Fmt.pr "- mrmime ok.\n%!" ;
    if dkim then verify_dkim maildir (match new_line with CRLF -> `CRLF | LF -> `LF) message ; v
  | Error (`Msg err) ->
    Fmt.pr "- mrmime error: %s.\n%!" err ;
    error_msgf "On %a: %s" Fmt.(using Maildir.value Maildir.pp_message) message err

let process verify_only_new_messages dkim maildir new_line acc message =
  if Maildir.is_new message || not verify_only_new_messages
  then
    match just_verify maildir dkim new_line message with
    | Error (`Msg err) ->
      Log.err (fun m -> m "Retrieve an error: %s." err) ;
      false && acc
    | Ok () -> true && acc
  else acc

let run () maildir_path host verify_only_new_messages dkim new_line =
  let maildir = Maildir.create ~pid:(Unix.getpid ()) ~host ~random maildir_path in
  let res = Maildir_unix.(fold (process verify_only_new_messages dkim maildir new_line) true fs maildir) in
  if res then Ok () else Rresult.R.error (`Msg "Retrieve an invalid e-mail")

open Cmdliner

let host =
  let doc = "Hostname of machine." in
  Arg.(value & opt Conv.hostname (Unix.gethostname ()) & info [ "h"; "host" ] ~docv:"<host>" ~doc)

let verify =
  let doc = "Verify only new messages." in
  Arg.(value & flag & info [ "only-new" ] ~doc)

let dkim =
  let doc = "Verify DKIM signatures." in
  Arg.(value & flag & info [ "dkim" ] ~doc)

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
        $ dkim
        $ Argument.new_line),
  Term.info "verify" ~doc ~exits ~man
