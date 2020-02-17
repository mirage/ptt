open Colombe.Sigs
open Rresult

type ('s, 'k) t
(** The {i authenticator} type. *)

type username = Emile.local
type 'k password = 'k Digestif.t

val v : (username -> 'k password -> (bool, 's) io) -> ('s, 'k) t
(** [v authenticator] makes an {i authenticator}. *)

type mechanism =
  | PLAIN of string option
  (** Type of mechanism used by the client. *)

val decode_authentication
  :  's impl
  -> 'k Digestif.hash
  -> mechanism
  -> ('s, 'k) t
  -> string
  -> ((bool, [> R.msg ]) result, 's) io
(** [decode_authentication scheduler hash mechanism t payload] tries to decode [payload]
   according [mechanism] used. Then, it applies the {i authenticator} [t] with decoded value.
   [hash] is used as a {i witness} of which hash algorithm we want to use. *)
