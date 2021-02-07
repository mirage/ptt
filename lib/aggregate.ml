let ( <.> ) f g x = f (g x)

module By_domain = Map.Make (struct
  type t = [ `host ] Domain_name.t

  let compare = Domain_name.compare
end)

module By_ipaddr = Map.Make (Ipaddr)

type unresolved_elt = [ `All | `Postmaster | `Local of Emile.local list ]
type resolved_elt = [ `All | `Local of Emile.local list ]

let postmaster = [`Atom "Postmaster"]
let equal_local = Emile.equal_local ~case_sensitive:true

let add_unresolved ~domain elt unresolved =
  match elt, By_domain.find_opt domain unresolved with
  | `All, _ -> By_domain.add domain `All unresolved
  | _, Some `All | `Postmaster, Some `Postmaster -> unresolved
  | `Postmaster, Some (`Local vs) ->
    if List.exists (equal_local postmaster) vs then unresolved
    else By_domain.add domain (`Local (postmaster :: vs)) unresolved
  | `Local v, Some (`Local vs) ->
    if List.exists (equal_local v) vs then unresolved
    else By_domain.add domain (`Local (v :: vs)) unresolved
  | `Local v, Some `Postmaster ->
    By_domain.add domain (`Local [v; postmaster]) unresolved
  | `Postmaster, None -> By_domain.add domain `Postmaster unresolved
  | `Local v, None -> By_domain.add domain (`Local [v]) unresolved

let add_resolved ipaddr elt resolved =
  match elt, By_ipaddr.find_opt ipaddr resolved with
  | `All, _ -> By_ipaddr.add ipaddr `All resolved
  | _, Some `All -> resolved
  | `Local v, Some (`Local vs) ->
    if List.exists (equal_local v) vs then resolved
    else By_ipaddr.add ipaddr (`Local (v :: vs)) resolved
  | `Local v, None -> By_ipaddr.add ipaddr (`Local [v]) resolved

let aggregate_by_domains ~domain =
  let open Colombe in
  let open Forward_path in
  let fold (unresolved, resolved) = function
    | Postmaster -> add_unresolved ~domain `Postmaster unresolved, resolved
    | Forward_path {Path.domain= Domain.Domain v; Path.local; _} ->
      let domain = Domain_name.(host_exn <.> of_strings_exn) v in
      let local = Colombe_emile.of_local local in
      add_unresolved ~domain (`Local local) unresolved, resolved
    | Domain (Domain.Domain v) ->
      let domain = Domain_name.(host_exn <.> of_strings_exn) v in
      add_unresolved ~domain `All unresolved, resolved
    | Domain (Domain.IPv4 v4) ->
      unresolved, add_resolved (Ipaddr.V4 v4) `All resolved
    | Domain (Domain.IPv6 v6) ->
      unresolved, add_resolved (Ipaddr.V6 v6) `All resolved
    | Forward_path {Path.domain= Domain.IPv4 v4; Path.local; _} ->
      let local = Colombe_emile.of_local local in
      unresolved, add_resolved (Ipaddr.V4 v4) (`Local local) resolved
    | Forward_path {Path.domain= Domain.IPv6 v6; Path.local; _} ->
      let local = Colombe_emile.of_local local in
      unresolved, add_resolved (Ipaddr.V6 v6) (`Local local) resolved
    | Domain (Domain.Extension _)
    | Forward_path {Path.domain= Domain.Extension _; _} ->
      unresolved, resolved in
  (* TODO *)
  List.fold_left fold (By_domain.empty, By_ipaddr.empty)
