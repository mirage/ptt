open Common

let src = Logs.Src.create "ttf-solystic.commit" ~doc:"logs ttf-solystic's commit event"
module Log = (val Logs.src_log src : Logs.LOG)

module Status = struct
  let ok = Fmt.const Fmt.string "[OK]" |> Fmt.styled `Green
  let error = Fmt.const Fmt.string "[ERROR]" |> Fmt.styled `Red
  let wait = Fmt.const Fmt.string "[...]" |> Fmt.styled `Yellow
end

let run () maildir_path host _flags _message =
  let maildir = Maildir.create ~pid:(Unix.getpid ()) ~host ~random maildir_path in
  match Maildir_unix.(verify fs maildir) with
  | false ->
    Fmt.pr "%a Invalid Maildir directory.\n%!" Status.error () ;
    Rresult.R.error_msgf "%a is not a valid Maildir directory." Fpath.pp maildir_path
  | true ->
    assert false
