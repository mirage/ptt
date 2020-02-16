open Colombe.Sigs
open Rresult

type ('s, 'k) t

type username = Emile.local
type 'k password = 'k Digestif.t

val v : (username -> 'k password -> (bool, 's) io) -> ('s, 'k) t

type mechanism =
  | PLAIN of string option

val decode_authentication : 's impl -> 'k Digestif.hash -> mechanism -> ('s, 'k) t -> string -> ((bool, [> R.msg ]) result, 's) io
