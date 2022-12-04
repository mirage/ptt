type t = {
    targets: Emile.mailbox list
  ; password: Digestif.BLAKE2B.t
  ; insecure: bool
}

val to_string_json : t -> string
val of_string_json : string -> (t, [> `Msg of string ]) result
val key_of_local : Emile.local -> Mirage_kv.Key.t
val local_of_key : Mirage_kv.Key.t -> Emile.local
