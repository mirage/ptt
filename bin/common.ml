open Cmdliner

type new_line =
  | CRLF | LF

module Conv = struct
  let new_line =
    let parser str = match String.lowercase_ascii str with
      | "crlf" -> Ok CRLF
      | "lf" -> Ok LF
      | str -> Rresult.R.error_msgf "Invalid newline: %s" str in
    let pp ppf = function
      | CRLF -> Fmt.string ppf "<CRLF>"
      | LF -> Fmt.string ppf "<LF>" in
    Arg.conv ~docv:"<newline>" (parser, pp)

  let path =
    let parser = Fpath.of_string in
    let pp = Fpath.pp in
    Arg.conv ~docv:"<path>" (parser, pp)

  let existing_path =
    let parser str = match Fpath.of_string str with
      | Ok v ->
        if Sys.file_exists (Fpath.to_string v)
        then Ok v
        else Rresult.R.error_msgf "%a not found" Fpath.pp v
      | Error _ as err -> err in
    let pp = Fpath.pp in
    Arg.conv ~docv:"<path>" (parser, pp)

  let existing_directory =
    let parser str = match Fpath.of_string str with
      | Ok v ->
        if Sys.file_exists (Fpath.to_string v)
        then if Sys.is_directory (Fpath.to_string v)
          then Ok v
          else Rresult.R.error_msgf "%a is not a directory" Fpath.pp v
        else Rresult.R.error_msgf "%a not found" Fpath.pp v
      | Error _ as err -> err in
    let pp = Fpath.pp in
    Arg.conv ~docv:"<path>" (parser, pp)

  let hostname =
    let sanitize_lwsp str =
      let is_lwsp = function '\r' | '\n' | ' ' | '\t' -> true | _ -> false in
      let len = String.length str in
      let res = Buffer.create len in
      for i = 0 to len - 1
      do if not (is_lwsp str.[i]) then Buffer.add_char res str.[i] done ;
      Buffer.contents res in
    let parser str =
      let str = sanitize_lwsp str in
      match Angstrom.parse_string Mrmime.Rfc5321.domain str with
      | Ok _ -> Ok str
      | Error err -> Rresult.R.error_msg err in
    let pp = Fmt.using sanitize_lwsp Fmt.string in
  Arg.conv ~docv:"<host>" (parser, pp)
end

module Commands = struct
  let setup_fmt_and_logs style_renderer log_level =
    Fmt_tty.setup_std_outputs ?style_renderer () ;
    Logs.set_level log_level ;
    Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ()) ;
    Logs.info (fun m -> m "ptt %%VERSION%% running.") ;
    `Ok ()
end

module Arguments = struct
  let new_line =
    let doc = "Kind of new line (RFC 822 or UNIX)." in
    Arg.(value & opt Conv.new_line LF & info [ "n"; "newline" ] ~docv:"<newline>" ~doc)

  let setup_fmt_and_logs =
    let style_renderer =
      let env = Arg.env_var "PTT_COLOR" in
      Fmt_cli.style_renderer ~docs:Manpage.s_common_options ~env () in
    let log_level =
      let env = Arg.env_var "PTT_VERBOSITY" in
      Logs_cli.level ~docs:Manpage.s_common_options ~env () in
    Term.(ret (const Commands.setup_fmt_and_logs $ style_renderer $ log_level))
end

let sub_string_and_replace_new_line chunk len =
  let count = ref 0 in
  String.iter (function '\n' -> incr count | _ -> ()) (Bytes.sub_string chunk 0 len) ;
  let plus = !count in
  let pos = ref 0 in
  let res = Bytes.create (len + plus) in
  for i = 0 to len - 1
  do match Bytes.unsafe_get chunk i with
    | '\n' ->
      Bytes.unsafe_set res !pos '\r' ;
      Bytes.unsafe_set res (!pos + 1) '\n' ;
      pos := !pos + 2
    | chr ->
      Bytes.unsafe_set res !pos chr ;
      incr pos
  done ; Bytes.unsafe_to_string res

let sanitize_input new_line chunk len = match new_line with
  | CRLF -> Bytes.sub_string chunk 0 len
  | LF -> sub_string_and_replace_new_line chunk len
