open Colombe.Sigs
open Rresult

let ( <.> ) f g x = f (g x)

type ('s, 'k) t = username -> 'k password -> (bool, 's) io
and username = Emile.local
and 'k password = 'k Digestif.t

external v : (username -> 'k password -> (bool, 's) io) -> ('s, 'k) t
  = "%identity"

let is_zero = ( = ) '\000'

let authenticate {return; bind} hash username password t =
  let ( >>= ) = bind in
  let p = Digestif.digest_string hash password in
  Bytes.fill (Bytes.unsafe_of_string password) 0 (String.length password) '\000'
  ; t username p >>= fun v -> return (R.ok v)

let decode_plain_authentication ({return; _} as scheduler) hash ?stamp t v =
  let parser =
    let open Angstrom in
    take_till is_zero >>= fun v0 ->
    char '\000' *> take_till is_zero >>= fun v1 ->
    char '\000' *> available >>= take >>= fun v2 -> return (v0, v1, v2) in
  match
    ( stamp
    , Base64.decode ~pad:false (* XXX(dinosaure): not really sure. *) v
      >>= (R.reword_error (fun _ -> `Msg "Invalid input")
          <.> Angstrom.parse_string ~consume:All parser) )
  with
  | Some stamp, Ok (v0, v1, v2) ->
    if Eqaf.equal stamp v0 then
      match Angstrom.parse_string ~consume:All Emile.Parser.local_part v1 with
      | Ok username -> authenticate scheduler hash username v2 t
      | Error _ -> return (R.error_msgf "Invalid username: %S" v1)
    else return (R.error_msgf "Invalid stamp")
  | None, Ok ("", v1, v2) -> (
    match Angstrom.parse_string ~consume:All Emile.Parser.local_part v1 with
    | Ok username -> authenticate scheduler hash username v2 t
    | Error _ -> return (R.error_msgf "Invalid username: %S" v1))
  | None, Ok (_, _, _) -> return (R.error_msgf "Unexpected stamp")
  | _, (Error _ as err) -> return err

type mechanism = PLAIN of string option

let decode_authentication scheduler hash m t v =
  match m with
  | PLAIN stamp -> decode_plain_authentication scheduler hash ?stamp t v
