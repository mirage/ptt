type t =
  | PLAIN

val pp : t Fmt.t
val of_string_exn : string -> t
val equal : t -> t -> bool
