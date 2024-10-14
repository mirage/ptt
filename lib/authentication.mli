open Rresult

type 'k t
(** The {i authenticator} type. *)

type username = [ `Dot_string of string list | `String of string ]
type 'k password = 'k Digestif.t

val v : (username -> 'k password -> bool Lwt.t) -> 'k t
(** [v authenticator] makes an {i authenticator}. *)

type mechanism =
  | PLAIN of string option  (** Type of mechanism used by the client. *)

val decode_authentication :
     'k Digestif.hash
  -> mechanism
  -> 'k t
  -> string
  -> (username * bool, [> R.msg ]) result Lwt.t
(** [decode_authentication scheduler hash mechanism t payload] tries to decode
    [payload] according [mechanism] used. Then, it applies the {i authenticator}
    [t] with decoded value. [hash] is used as a {i witness} of which hash
    algorithm we want to use. *)
