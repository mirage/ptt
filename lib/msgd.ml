open Colombe

type from = Reverse_path.t * (string * string option) list
type recipient = Forward_path.t * (string * string option) list

type key =
  { domain_from: Domain.t
  ; from: from
  ; recipients: recipient list
  ; id: int64
  ; ip: Ipaddr.t }

let domain_from {domain_from; _} = domain_from
let from {from; _} = from
let recipients {recipients; _} = recipients
let id {id; _} = id
let ipaddr {ip; _} = ip

let key ~domain_from ~from ~recipients ~ipaddr:ip id =
  {domain_from; from; recipients; id; ip}

let pp ppf key =
  Fmt.pf ppf
    "{ @[<hov>domain_from= %a;@ from= %a;@ recipients= @[<hov>%a@];@ id= \
     %Ld;@] }"
    Domain.pp key.domain_from Reverse_path.pp (fst key.from)
    Fmt.(Dump.list Forward_path.pp)
    (List.map fst key.recipients)
    key.id

let equal_recipients a b =
  let a = List.sort Forward_path.compare a in
  let b = List.sort Forward_path.compare b in
  let rec go a b =
    match a, b with
    | _ :: _, [] -> false
    | [], _ :: _ -> false
    | a :: ar, b :: br ->
      let res = Forward_path.equal a b in
      if res then go ar br else false
    | [], [] -> true in
  go a b

let equal a b =
  Domain.equal a.domain_from b.domain_from
  && Reverse_path.equal (fst a.from) (fst b.from)
  && equal_recipients (List.map fst a.recipients) (List.map fst b.recipients)
  && a.id = b.id
  && Ipaddr.compare a.ip b.ip = 0

let src = Logs.Src.create "ptt.messaged"

module Log = (val Logs.src_log src)

type error =
  [ `Aborted
  | `Not_enough_memory
  | `Too_big
  | `Failed
  | `Requested_action_not_taken of [ `Temporary | `Permanent ] ]

type result = [ error | `Ok ]

let pp_error ppf = function
  | `Aborted -> Fmt.string ppf "Aborted"
  | `Not_enough_memory -> Fmt.string ppf "Not enough memory"
  | `Too_big -> Fmt.string ppf "Email too big"
  | `Failed -> Fmt.string ppf "Failed"
  | `Requested_action_not_taken `Temporary ->
      Fmt.string ppf "Requested action not taken (temporary)"
  | `Requested_action_not_taken `Permanent ->
      Fmt.string ppf "Requested action not taken (permanent)"

type t = (key * string Lwt_stream.t * result Lwt.u) Lwt_stream.t
