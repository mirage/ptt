type key = Dns.Mx.t

val pp_key : key Fmt.t

include Map.S with type key := key

val v : preference:int -> domain:[ `host ] Domain_name.t -> Ipaddr.t -> Ipaddr.t t
val vs : (Dns.Mx.t * Ipaddr.t) list -> Ipaddr.t t
