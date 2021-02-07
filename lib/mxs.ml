type elt = {
    preference: int
  ; mx_ipaddr: Ipaddr.t
  ; mx_domain: [ `host ] Domain_name.t option
}

let pp_elt : elt Fmt.t =
 fun ppf elt ->
  Fmt.pf ppf "{ @[<hov>preference= %d;@ mx_ipaddr= %a;@ mx_domain= %a;@] }"
    elt.preference Ipaddr.pp elt.mx_ipaddr
    (Fmt.option Domain_name.pp)
    elt.mx_domain

module Elt = struct
  type t = elt

  let compare {mx_ipaddr= a; _} {mx_ipaddr= b; _} = Ipaddr.compare a b
end

include (Set.Make (Elt) : Set.S with type elt := elt)

let v ~preference ?domain ipaddr =
  {preference; mx_domain= domain; mx_ipaddr= ipaddr}
