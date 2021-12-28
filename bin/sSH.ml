open Rresult

type flow = {ic: in_channel; oc: out_channel}
type error = [ `Msg of string ]

let pp_error ppf (`Msg err) = Fmt.string ppf err

type write_error = [ `Msg of string | `Closed ]

let pp_write_error ppf = function
  | `Closed -> Fmt.string ppf "Connection closed by peer."
  | `Msg err -> Fmt.string ppf err

type endpoint = string * string * string * [ `Rd | `Wr ]

let connect (user, host, repository, action) =
  let target = Fmt.str "%s@%s" user host in
  let cmd =
    match action with
    | `Rd -> Fmt.str "git-upload-pack '%s'" repository
    | `Wr -> Fmt.str "git-receive-pack '%s'" repository in
  try
    let ic, oc = Unix.open_process_args "/usr/bin/ssh" [|"ssh"; target; cmd|] in
    Lwt.return_ok {ic; oc}
  with
  | Unix.Unix_error (err, f, arg) ->
    R.error_msgf "%s(%s): %s" f arg (Unix.error_message err) |> Lwt.return
  | exn -> raise exn

let read flow =
  let res = Bytes.create 0x1000 in
  match input flow.ic res 0 (Bytes.length res) with
  | 0 -> Lwt.return_ok `Eof
  | len -> Lwt.return_ok (`Data (Cstruct.of_bytes res ~off:0 ~len))
  | exception End_of_file -> Lwt.return_ok `Eof

let write flow cs =
  output_string flow.oc (Cstruct.to_string cs)
  ; flush flow.oc
  ; Lwt.return_ok ()

open Lwt.Infix

let rec writev flow = function
  | [] -> Lwt.return_ok ()
  | x :: r -> (
    write flow x >>= function
    | Ok () -> writev flow r
    | Error _ as err -> Lwt.return err)

let close flow = close_in flow.ic ; close_out flow.oc ; Lwt.return_unit
