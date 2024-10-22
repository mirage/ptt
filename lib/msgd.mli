open Colombe

type from = Reverse_path.t * (string * string option) list
type recipient = Forward_path.t * (string * string option) list
type key

val domain_from : key -> Domain.t
val from : key -> from
val recipients : key -> recipient list
val id : key -> int64
val ipaddr : key -> Ipaddr.t
val pp : key Fmt.t
val equal : key -> key -> bool

val key :
     domain_from:Domain.t
  -> from:from
  -> recipients:recipient list
  -> ipaddr:Ipaddr.t
  -> int64
  -> key

type error =
  [ `Aborted
  | `Not_enough_memory
  | `Too_big
  | `Failed
  | `Requested_action_not_taken of [ `Temporary | `Permanent ] ]

type result = [ error | `Ok ]

type t = (key * string Lwt_stream.t * result Lwt.u) Lwt_stream.t

val pp_error : error Fmt.t
