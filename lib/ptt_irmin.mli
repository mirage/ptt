type t = {targets: Emile.mailbox list; password: Digestif.BLAKE2B.t}

include Irmin.Contents.S with type t := t

val key_of_local : Emile.local -> Mirage_kv.Key.t
val local_of_key : Mirage_kv.Key.t -> Emile.local
