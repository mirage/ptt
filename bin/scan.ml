open Common

let pp_kind ppf stat = match stat.Unix.st_kind with
  | Unix.S_REG -> Fmt.string ppf "-"
  | Unix.S_DIR -> Fmt.string ppf "d"
  | Unix.S_CHR -> Fmt.string ppf "c"
  | Unix.S_BLK -> Fmt.string ppf "b"
  | Unix.S_LNK -> Fmt.string ppf "l"
  | Unix.S_FIFO -> Fmt.string ppf "p"
  | Unix.S_SOCK -> Fmt.string ppf "s"

let pp_perm ppf stat =
  let r_perm = Fmt.using (function 0 -> "-" | _ -> "r") Fmt.string in
  let w_perm = Fmt.using (function 0 -> "-" | _ -> "w") Fmt.string in
  let x_perm = Fmt.using (function 0 -> "-" | _ -> "x") Fmt.string in
  Fmt.pf ppf "%a%a%a%a%a%a%a%a%a"
    r_perm (stat.Unix.st_perm land 0x100)
    w_perm (stat.Unix.st_perm land 0x80 )
    x_perm (stat.Unix.st_perm land 0x40 )
    r_perm (stat.Unix.st_perm land 0x20 )
    w_perm (stat.Unix.st_perm land 0x10 )
    x_perm (stat.Unix.st_perm land 0x8  )
    r_perm (stat.Unix.st_perm land 0x4  )
    w_perm (stat.Unix.st_perm land 0x2  )
    x_perm (stat.Unix.st_perm land 0x1  )

let pp_user ppf stat =
  let user = Unix.getpwuid stat.Unix.st_uid in
  Fmt.string ppf user.Unix.pw_name

let pp_group ppf stat =
  let group = Unix.getgrgid stat.Unix.st_gid in
  Fmt.string ppf group.Unix.gr_name

let pp_time =
  let pp = Fmt.(option ~none:(const string "#invalid-date") Ptime.pp) in
  Fmt.using Ptime.of_float_s pp

let pp_size ppf stat = Fmt.int ppf stat.Unix.st_size
let pp_pad n pp = Fmt.using (pad n <.> Fmt.strf "%a" pp) Fmt.string

let parse_header new_line ic =
  let open Angstrom.Buffered in
  let raw = Bytes.create 4096 in
  let rec go = function
    | Partial continue ->
      let len = input ic raw 0 (Bytes.length raw) in
      let raw = sanitize_input new_line raw len in
      let res = if len = 0 then continue `Eof else continue (`String raw) in
      go res
    | Done (_, header) -> close_in ic ; Ok header
    | Fail (_, _, err) -> close_in ic ; Error (`Msg err) in
  go (parse Mrmime.Mail.header)

let get_field
  : Mrmime.Field_name.t -> Mrmime.Header.t -> (Mrmime.Header.value list, [ `Not_found | Rresult.R.msg ]) result
  = fun field header ->
    match Mrmime.Header.get field header with
    | [] -> Error `Not_found
    | lst -> Ok (List.map (fun (_, v, _) -> v) lst)

let print_fields maildir new_line message fields =
  let open Rresult.R in
  match in_channel_of_message maildir message >>= parse_header new_line with
  | Ok (_content, header, _resents, _traces, _rest) ->
    let values = List.map (fun field -> field, get_field field header) fields in
    List.iter
      (function
        | field, Ok values ->
          List.iter
            (Fmt.pr "@[<1>%a:@ %a@]\n%!" Mrmime.Field_name.pp field Mrmime.Header.pp_value)
            values
        | _, Error (`Msg err) -> Fmt.pr "%a: %s.\n%!" Pretty_printer.pp_error () err
        | field, Error `Not_found ->
          Fmt.pr "%a: %a not found.\n%!"
            Pretty_printer.pp_error ()
            Mrmime.Field_name.pp field)
      values
  | Error _ ->
    Fmt.pr "%a: header of %a can not be parser"
      Pretty_printer.pp_error () Fmt.(using Maildir.value Maildir.pp_message) message

let pp maildir new_line fields () message =
  let path = Maildir_unix.get maildir message in
  let stat = Unix.stat (Fpath.to_string path) in
  Fmt.pr "%a%a %a %a %a %a %a\n%!"
    pp_kind stat
    pp_perm stat
    (pp_pad 10 pp_user) stat
    (pp_pad 10 pp_group) stat
    (pp_pad 10 pp_size) stat
    pp_time stat.Unix.st_mtime
    Fmt.(using Maildir.value Maildir.pp_message) message ;
  if List.length fields > 0
  then print_fields maildir new_line message fields ; ()

let run () maildir_path host only_new new_line fields =
  let maildir = Maildir.create ~pid:(Unix.getpid ()) ~host ~random maildir_path in

  if only_new
  then Maildir_unix.scan_only_new (pp maildir new_line fields) () Maildir_unix.fs maildir
  else Maildir_unix.fold (pp maildir new_line fields) () Maildir_unix.fs maildir ;

  Ok ()

open Cmdliner

let host =
  let doc = "Hostname of machine." in
  Arg.(value & opt Conv.hostname (Unix.gethostname ()) & info [ "h"; "host" ] ~docv:"<host>" ~doc)

let only_new =
  let doc = "Lists only new messages." in
  Arg.(value & flag & info [ "only-new" ] ~doc)

let fields =
  let doc = "Fields to extract." in
  Arg.(value & opt (list Conv.field) [] & info [ "w"; "with" ] ~docv:"<fields>" ~doc)

let command =
  let doc = "Scan tool." in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Lists mails available on <maildir> database." ] in
  Term.(pure run
        $ Argument.setup_fmt_and_logs
        $ Argument.maildir_path
        $ host
        $ only_new
        $ Argument.new_line
        $ fields),
  Term.info "scan" ~doc ~exits ~man
