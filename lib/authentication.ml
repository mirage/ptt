open Rresult
open Lwt.Infix

let ( <.> ) f g x = f (g x)

type 'k t = username -> 'k password -> bool Lwt.t
and username = [ `Dot_string of string list | `String of string ]
and 'k password = 'k Digestif.t

external v : (username -> 'k password -> bool Lwt.t) -> 'k t = "%identity"

let is_zero = ( = ) '\000'

let authenticate hash username password t =
  let p = Digestif.digest_string hash password in
  Bytes.fill (Bytes.unsafe_of_string password) 0 (String.length password) '\000';
  t username p >>= fun v -> Lwt.return_ok (username, v)

let decode_plain_authentication hash ?stamp t v =
  let ( >>= ) = Result.bind in
  let parser =
    let open Angstrom in
    take_till is_zero >>= fun v0 ->
    char '\000' *> take_till is_zero >>= fun v1 ->
    char '\000' *> available >>= take >>= fun v2 -> return (v0, v1, v2) in
  let payload =
    Base64.decode ~pad:false (* XXX(dinosaure): not really sure. *) v
    >>= (R.reword_error (fun _ -> `Msg "Invalid input")
        <.> Angstrom.parse_string ~consume:All parser) in
  match stamp, payload with
  | Some stamp, Ok (v0, v1, v2) ->
    if Eqaf.equal stamp v0 then
      match
        Angstrom.parse_string ~consume:All Colombe.Path.Decoder.local_part v1
      with
      | Ok username -> authenticate hash username v2 t
      | Error _ -> Lwt.return (R.error_msgf "Invalid username: %S" v1)
    else Lwt.return (R.error_msgf "Invalid stamp")
  | None, Ok ("", v1, v2) -> begin
    match
      Angstrom.parse_string ~consume:All Colombe.Path.Decoder.local_part v1
    with
    | Ok username -> authenticate hash username v2 t
    | Error _ -> Lwt.return (R.error_msgf "Invalid username: %S" v1)
  end
  | None, Ok (_, _, _) -> Lwt.return (R.error_msgf "Unexpected stamp")
  | _, (Error _ as err) -> Lwt.return err

type mechanism = PLAIN of string option

let decode_authentication hash m t v =
  match m with PLAIN stamp -> decode_plain_authentication hash ?stamp t v
