let src = Logs.Src.create "ptt.map"

module Log = (val Logs.src_log src)

module By_domain = Map.Make (struct
  type t = [ `host ] Domain_name.t

  let compare = Domain_name.compare
end)

type t =
  { postmaster: Emile.mailbox
  ; map: (local, Colombe.Forward_path.t list) Hashtbl.t }
and local = [ `Dot_string of string list | `String of string ]

let postmaster {postmaster; _} = postmaster
let empty ~postmaster = {postmaster; map= Hashtbl.create 256}

let add ~local destination t =
  match Colombe_emile.to_forward_path destination with
  | Error (`Msg err) -> invalid_arg err
  | Ok destination ->
    match Hashtbl.find_opt t.map local with
    | Some vs ->
      if not (List.exists (Colombe.Forward_path.equal destination) vs)
      then Hashtbl.replace t.map local (destination :: vs)
    | None -> Hashtbl.add t.map local [ destination ]

let exists_as_sender sender ~info t =
  match sender with
  | None -> false
  | Some {Colombe.Path.local; domain; _} ->
    Colombe.Domain.equal domain info.Ptt_common.domain
    && Hashtbl.mem t.map local

let recipients ~local {map; _} =
  Hashtbl.find_opt map local
  |> Option.value ~default:[]

let all t = Hashtbl.fold (fun _ -> List.rev_append) t.map []

let ( $ ) f g x = f (g x)

module Set = Set.Make (Colombe.Forward_path)

let expand ~info t recipients =
  let open Colombe in
  let open Forward_path in
  List.map (function
    | Postmaster ->
      (Result.to_option $ Colombe_emile.to_forward_path) t.postmaster
      |> Option.value ~default:Postmaster
      |> (Fun.flip List.cons [])
    | Domain domain as recipient ->
      if Domain.equal domain info.Ptt_common.domain
      then all t
      else [ recipient ]
    | Forward_path { Path.local; domain; _ } as recipient ->
      if Domain.equal domain info.Ptt_common.domain
      then match Hashtbl.find_opt t.map local with
        | Some recipients -> recipients
        | None -> []
      else [ recipient ]) recipients
  |> List.concat
  |> Set.of_list |> Set.elements
