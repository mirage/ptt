type t =
  | PLAIN

let pp ppf = function
  | PLAIN -> Fmt.string ppf "PLAIN"

let of_string_exn x = match String.lowercase_ascii x with
  | "plain" -> PLAIN
  | m -> Fmt.invalid_arg "Invalid mechanism: %s" m

let equal a b = match a, b with
  | PLAIN, PLAIN -> true
