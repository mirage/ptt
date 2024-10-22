type key = Dns.Mx.t

val pp_key : key Fmt.t

include Map.S with type key := key

val v : preference:int -> domain:[ `host ] Domain_name.t -> Ipaddr.t list -> Ipaddr.t list t
val vs : (Dns.Mx.t * Ipaddr.t list) list -> Ipaddr.t list t
