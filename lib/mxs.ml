type elt =
  { preference : int
  ; mx_ipaddr : Ipaddr.t
  ; mx_domain : [ `host ] Domain_name.t option }

module Elt = struct
  type t = elt

  let compare
      { mx_ipaddr= a; _ }
      { mx_ipaddr= b; _ } =
    Ipaddr.compare a b
end

include (Set.Make(Elt) : Set.S with type elt := elt)

let v ~preference ?domain ipaddr =
  { preference
  ; mx_domain= domain
  ; mx_ipaddr= ipaddr }
