open Cmdliner

type new_line =
  | CRLF | LF

let (<.>) f g = fun x -> f (g x)
let random = Int64.of_int <.> Random.bits

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

  let field =
    let parser = Mrmime.Header.Field.of_string in
    let pp = Mrmime.Header.Field.pp in
    Arg.conv ~docv:"<field>" (parser, pp)

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

  let message =
    let parser = Maildir.of_filename in
    let pp = Maildir.pp_message in
    Arg.conv ~docv:"<message>" (parser, pp)
end

module Commands = struct
  let setup_fmt_and_logs style_renderer log_level =
    Fmt_tty.setup_std_outputs ?style_renderer () ;
    Logs.set_level log_level ;
    Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ()) ;
    Logs.info (fun m -> m "ptt %%VERSION%% running.") ;
    `Ok ()

  let maildir_verify maildir_path =
    let fake_maildir =
      Maildir.create
        ~pid:(Unix.getpid ())
        ~host:(Unix.gethostname ())
        ~random
        maildir_path in
    match Maildir_unix.(verify fs fake_maildir) with
    | true -> `Ok maildir_path
    | false ->
      let err = Fmt.strf "Invalid <maildir> path: %a." Fpath.pp maildir_path in
      `Error (false, err)
end

module Pp = struct
  let pp_phrase =
    let pp_word ppf = function `Atom x -> Fmt.string ppf x | `String x -> Fmt.pf ppf "%S" x in
    let pp_elt ppf = function
      | `Dot -> Fmt.string ppf "."
      | `Word x -> pp_word ppf x
      | `Encoded { Mrmime.Encoded_word.data = Ok raw; _ } ->
        (* Handle output, UTF-8, ISO-8859? *)
        Fmt.string ppf raw
      | `Encoded _ -> Fmt.string ppf "<?>" in
    Fmt.(list ~sep:(const string " ") pp_elt)

  let pp_mailbox ppf mailbox =
    let open Mrmime.Mailbox in
    let pp_word ppf = function `Atom x -> Fmt.string ppf x | `String x -> Fmt.pf ppf "%S" x in
    let pp_literal_domain ppf = function
      | IPv4 x -> Fmt.pf ppf "[%a]" Ipaddr.V4.pp x
      | IPv6 x -> Fmt.pf ppf "[IPv6:%a]" Ipaddr.V6.pp x
      | Ext (ldh, v) -> Fmt.pf ppf "[%s:%s]" ldh v in
    let pp_domain ppf = function
      | `Domain lst -> Fmt.(list ~sep:(const string ".") string) ppf lst
      | `Literal x -> Fmt.pf ppf "[%s]" x
      | `Addr x -> pp_literal_domain ppf x in
    let pp_local = Fmt.(list ~sep:(const string ".") pp_word) in
    match mailbox.name, mailbox.domain with
    | Some name, (domain, []) ->
      Fmt.pf ppf "%a <%a@%a>" pp_phrase name pp_local mailbox.local pp_domain domain
    | Some name, (domain, rest) ->
      Fmt.pf ppf "%a <%a:%a@%a>"
        pp_phrase name
        Fmt.(list ~sep:(const string ",") (prefix (const string "@") pp_domain)) rest
        pp_local mailbox.local
        pp_domain domain
    | None, (domain, []) ->
      Fmt.pf ppf "<%a@%a>" pp_local mailbox.local pp_domain domain
    | None, (domain, rest) ->
      Fmt.pf ppf "<%a:%a@%a>"
        Fmt.(list ~sep:(const string ",") (prefix (const string "@") pp_domain)) rest
        pp_local mailbox.local
        pp_domain domain

  let pp_date ppf date =
    let open Mrmime.Date in
    let (dd, mm, yy) = date.date in
    let (h, m, s) = date.time in
    let pp_year ppf x =
      if x < 1000 then Fmt.pf ppf "%02d" x
      else Fmt.pf ppf "%d" x in
    match date.day with
    | Some day ->
      Fmt.pf ppf "%a %02d %a %a %02d:%02d%a %s"
        Day.pp day dd Month.pp mm pp_year yy h m Fmt.(option (prefix (const string ":") (Fmt.fmt "%02d"))) s
        (Zone.to_string date.zone)
    | None ->
      Fmt.pf ppf "%02d %a %a %02d:%02d%a %s"
        dd Month.pp mm pp_year yy h m Fmt.(option (prefix (const string ":") (Fmt.fmt "%02d"))) s
        (Zone.to_string date.zone)

  let pp_group ppf group =
    let open Mrmime.Group in
    Fmt.pf ppf "%a: %a" pp_phrase group.name Fmt.(list ~sep:(const string ", ") pp_mailbox) group.mailboxes

  let pp_address ppf = function
    | `Group group -> pp_group ppf group
    | `Mailbox mailbox -> pp_mailbox ppf mailbox

  let pp_unstructured ppf x =
    let pp_elt ppf = function
      | `Text x -> Fmt.string ppf x
      | `WSP _ -> Fmt.string ppf " "
      | `CRLF | `CR _ | `LF _ -> Fmt.nop ppf ()
      | `Encoded { Mrmime.Encoded_word.data = Ok raw; _ } ->
        (* Handle output, UTF-8, ISO-8859? *)
        Fmt.string ppf raw
      | `Encoded _ -> Fmt.string ppf "<?>" in
    Fmt.list pp_elt ppf x
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

  let maildir_path =
    let maildir_path =
      let doc = "Path of <maildir> directory." in
      let env = Arg.env_var "PTT_MAILDIR" in
      Arg.(required
           & opt (some Conv.existing_directory) None
           & info [ "m"; "maildir" ] ~docv:"<maildir>" ~doc ~env) in
    Term.(ret (const Commands.maildir_verify $ maildir_path))
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

let pad n x =
  if String.length x > n
  then x
  else x ^ String.make (n - String.length x) ' '

let in_channel_of_message maildir message =
  let m = Maildir.to_fpath maildir message in
  if Sys.file_exists (Fpath.to_string m)
  then if not (Sys.is_directory (Fpath.to_string m))
    then Rresult.R.ok (open_in (Fpath.to_string m))
    else Rresult.R.error_msgf "%a is a directory, expected a file." Fpath.pp m
  else Rresult.R.error_msgf "%a does not exists" Fpath.pp m
