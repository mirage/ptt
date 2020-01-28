(** A domain can have several Mail eXchange services ordered by
    a preference number. The lower preference is a higher priority.
    This module provides a way to store them. The uniq identifier for
    each MX value is the [Ipaddr.V4.t]. *)

type elt =
  { preference : int
  ; mx_ipaddr : Ipaddr.t
  ; mx_domain : [ `host ] Domain_name.t option }
(** Type of a MX value. *)

val v : preference:int -> ?domain:[ `host ] Domain_name.t -> Ipaddr.t -> elt
(** [v ~preference ?domain mx_ipaddr] returns an MX value. *)

include Set.S with type elt := elt
