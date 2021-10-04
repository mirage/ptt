(* XXX(dinosaure): we should be able to have different views
 * from this object such as the relay cares only about local
 * and targets and submission cares about password. *)

(* XXX(dinosaure): currently, we have the ability to get different
 * views if they share the same [pre_hash] function. But, such trick
 * is incompatible with a symchronization _in the fligth_.
 *
 * However, according to the current design of [ptt], it's currently
 * not easy to upgrade the local database _in the flight_ - due to
 * the functional aspect of the [Relay_map]. So it's probably possible
 * to apply the trick but not really sure that it is the best idea due
 * to implications. *)

open Rresult

let ( <.> ) f g x = f (g x)

let mailbox =
  Irmin.Type.(map string) (R.get_ok <.> Emile.of_string) Emile.to_string

type t = {
    targets: Emile.mailbox list
  ; password: Digestif.BLAKE2B.t
  ; insecure: bool
}

let blake2b =
  Irmin.Type.(map string) Digestif.BLAKE2B.of_hex Digestif.BLAKE2B.to_hex

let key_of_local local =
  let pp ppf = function
    | `Atom x -> Fmt.string ppf x
    | `String str -> Fmt.pf ppf "%S" str in
  let str = Fmt.str "%a" Fmt.(list ~sep:(always ".") pp) local in
  Mirage_kv.Key.v str

let local_of_key key =
  let str = Mirage_kv.Key.to_string key in
  match
    Angstrom.parse_string ~consume:All
      Angstrom.(char '/' *> Emile.Parser.local_part)
      str
  with
  | Ok v -> v
  | Error _ -> Fmt.failwith "Invalid local-part: %S" str

let t =
  let open Irmin.Type in
  record "relay" (fun targets password insecure ->
      {targets; password; insecure})
  |+ field "targets" (list mailbox) (fun t -> t.targets)
  |+ field "password" blake2b (fun t -> t.password)
  |+ field "insecure" bool (fun t -> t.insecure)
  |> sealr

let merge = Irmin.Merge.(option (idempotent t))
