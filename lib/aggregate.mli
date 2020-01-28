module By_domain : Map.S with type key = [ `host ] Domain_name.t
module By_ipaddr : Map.S with type key = Ipaddr.t

type unresolved_elt = [ `All | `Postmaster | `Local of Emile.local list ]
type resolved_elt = [ `All | `Local of Emile.local list ]

val add_unresolved : domain:[ `host ] Domain_name.t -> [ `All | `Postmaster | `Local of Emile.local ] -> unresolved_elt By_domain.t -> unresolved_elt By_domain.t
val add_resolved : Ipaddr.t -> [ `All | `Local of Emile.local ] -> resolved_elt By_ipaddr.t -> resolved_elt By_ipaddr.t
val aggregate_by_domains : domain:[ `host ] Domain_name.t -> Colombe.Forward_path.t list -> (unresolved_elt By_domain.t * resolved_elt By_ipaddr.t)
