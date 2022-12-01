open Lwt.Infix

type error = Unix.error * string * string
type write_error = [ `Closed | `Error of Unix.error * string * string ]

let pp_error ppf (err, f, v) =
  Fmt.pf ppf "%s(%s): %s" f v (Unix.error_message err)

let pp_write_error ppf = function
  | `Closed -> Fmt.pf ppf "Connection closed by peer"
  | `Error (err, f, v) -> Fmt.pf ppf "%s(%s): %s" f v (Unix.error_message err)

type flow = {ic: in_channel; oc: out_channel}
type endpoint = {user: string; path: string; host: Unix.inet_addr; port: int}

let pp_inet_addr ppf inet_addr =
  Fmt.string ppf (Unix.string_of_inet_addr inet_addr)

let connect {user; path; host; port} =
  let edn = Fmt.str "%s@%a" user pp_inet_addr host in
  let cmd = Fmt.str {sh|git-upload-pack '%s'|sh} path in
  let cmd = Fmt.str "ssh -p %d %s %a" port edn Fmt.(quote string) cmd in
  try
    let ic, oc = Unix.open_process cmd in
    Lwt.return_ok {ic; oc}
  with Unix.Unix_error (err, f, v) -> Lwt.return_error (`Error (err, f, v))

let read t =
  let tmp = Bytes.create 0x1000 in
  try
    let len = input t.ic tmp 0 0x1000 in
    if len = 0 then Lwt.return_ok `Eof
    else Lwt.return_ok (`Data (Cstruct.of_bytes tmp ~off:0 ~len))
  with Unix.Unix_error (err, f, v) -> Lwt.return_error (err, f, v)

let write t cs =
  let str = Cstruct.to_string cs in
  try output_string t.oc str ; flush t.oc ; Lwt.return_ok ()
  with Unix.Unix_error (err, f, v) -> Lwt.return_error (`Error (err, f, v))

let writev t css =
  let rec go t = function
    | [] -> Lwt.return_ok ()
    | x :: r -> (
      write t x >>= function
      | Ok () -> go t r
      | Error _ as err -> Lwt.return err) in
  go t css

let close t = close_in t.ic ; close_out t.oc ; Lwt.return_unit
