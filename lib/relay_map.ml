let src = Logs.Src.create "ptt.relay-map"

module Log = (val Logs.src_log src)

type t = {
    postmaster: Emile.mailbox
  ; domain: [ `host ] Domain_name.t
  ; map: (Emile.local, Colombe.Forward_path.t list) Hashtbl.t
}

let postmaster {postmaster; _} = postmaster
let domain {domain; _} = domain
let empty ~postmaster ~domain = {postmaster; domain; map= Hashtbl.create 256}

let add ~local mailbox t =
  match Colombe_emile.to_forward_path mailbox with
  | Error (`Msg err) -> invalid_arg err
  | Ok mailbox -> (
    Log.debug (fun m ->
        m "Add %a with %a." Emile.pp_local local Colombe.Forward_path.pp mailbox)
    ; try
        let rest = Hashtbl.find t.map local in
        if not (List.exists (Colombe.Forward_path.equal mailbox) rest) then
          Hashtbl.add t.map local (mailbox :: rest)
        ; t
      with Not_found ->
        Hashtbl.add t.map local [mailbox]
        ; t)

let exists reverse_path t =
  match reverse_path with
  | None -> false
  | Some {Colombe.Path.local; domain= Domain vs; _} ->
    let domain' = Domain_name.(host_exn (of_strings_exn vs)) in
    Domain_name.equal t.domain domain'
    && Hashtbl.mem t.map (Colombe_emile.of_local local)
  | _ -> false

let recipients ~local {map; _} =
  match Hashtbl.find map local with
  | recipients -> recipients
  | exception Not_found ->
    Log.err (fun m -> m "%a not found into our local map." Emile.pp_local local)
    ; []

let all t = Hashtbl.fold (fun _ vs a -> vs @ a) t.map []
let ( <.> ) f g x = f (g x)

let expand t unresolved resolved =
  let open Aggregate in
  let fold domain elt (unresolved, resolved) =
    if not (Domain_name.equal domain t.domain) then
      By_domain.add domain elt unresolved, resolved
    else
      let open Colombe in
      let open Forward_path in
      let fold (unresolved, resolved) = function
        | Postmaster -> (
          let local = t.postmaster.Emile.local in
          let domain, _ = t.postmaster.Emile.domain in
          match domain with
          | `Domain vs ->
            let domain = Domain_name.(host_exn <.> of_strings_exn) vs in
            add_unresolved ~domain `Postmaster unresolved, resolved
          | `Addr (Emile.IPv4 v4) ->
            unresolved, add_resolved (Ipaddr.V4 v4) (`Local local) resolved
          | `Addr (Emile.IPv6 v6) ->
            unresolved, add_resolved (Ipaddr.V6 v6) (`Local local) resolved
          | `Addr (Emile.Ext _) | `Literal _ -> unresolved, resolved)
        | Domain (Domain.Domain v) ->
          let domain = Domain_name.(host_exn <.> of_strings_exn) v in
          add_unresolved ~domain `All unresolved, resolved
        | Forward_path {Path.domain= Domain.Domain v; Path.local; _} ->
          let domain = Domain_name.(host_exn <.> of_strings_exn) v in
          let local = Colombe_emile.of_local local in
          add_unresolved ~domain (`Local local) unresolved, resolved
        | Forward_path {Path.domain= Domain.IPv4 v4; Path.local; _} ->
          let local = Colombe_emile.of_local local in
          unresolved, add_resolved (Ipaddr.V4 v4) (`Local local) resolved
        | Forward_path {Path.domain= Domain.IPv6 v6; Path.local; _} ->
          let local = Colombe_emile.of_local local in
          unresolved, add_resolved (Ipaddr.V6 v6) (`Local local) resolved
        | Domain (Domain.IPv4 v4) ->
          unresolved, add_resolved (Ipaddr.V4 v4) `All resolved
        | Domain (Domain.IPv6 v6) ->
          unresolved, add_resolved (Ipaddr.V6 v6) `All resolved
        | Forward_path {Path.domain= Domain.Extension _; _} ->
          unresolved, resolved
        | Domain (Domain.Extension _) -> unresolved, resolved in
      match elt with
      | `Postmaster -> List.fold_left fold (unresolved, resolved) [Postmaster]
      | `All -> List.fold_left fold (unresolved, resolved) (all t)
      | `Local vs ->
        Log.debug (fun m ->
            m "Replace locals %a by their destinations."
              Fmt.(Dump.list Emile.pp_local)
              vs)
        ; let vs =
            List.fold_left (fun a local -> recipients ~local t @ a) [] vs in
          List.fold_left fold (unresolved, resolved) vs in
  By_domain.fold fold unresolved (By_domain.empty, resolved)
