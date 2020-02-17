(** Aggregate module.

   The goal of this module is to split recipients into 2 sets:
   {ul
   {- [By_domain] which contains recipients with domains.}
   {- [By_ipaddr] which contains recipients with [Ipaddr.t] as a domain.}}

   This module is relevant when you look into [Relay_map] which is able to
   translate recipients with domains to others recipients. *)

module By_domain : Map.S with type key = [ `host ] Domain_name.t
module By_ipaddr : Map.S with type key = Ipaddr.t

type unresolved_elt = [ `All | `Postmaster | `Local of Emile.local list ]
type resolved_elt = [ `All | `Local of Emile.local list ]

val add_unresolved
  :  domain:[ `host ] Domain_name.t
  -> [ `All | `Postmaster | `Local of Emile.local ]
  -> unresolved_elt By_domain.t
  -> unresolved_elt By_domain.t

val add_resolved
  :  Ipaddr.t
  -> [ `All | `Local of Emile.local ]
  -> resolved_elt By_ipaddr.t
  -> resolved_elt By_ipaddr.t

val aggregate_by_domains
  :  domain:[ `host ] Domain_name.t
  -> Colombe.Forward_path.t list
  -> (unresolved_elt By_domain.t * resolved_elt By_ipaddr.t)
