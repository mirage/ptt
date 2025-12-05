(* The mailing list manager *)

(* TODO: the queues (pending_subscriptions, pending_moderator_subscriptions, pending_mails) need to be freed after some time (or explicitly). *)

(* All the automated mails as well need to have the return-path to be xx-return-... (for bounce processing) *)
type t = {
  name : string ;
  moderators : string list ;
  pending_subscriptions : ([ `Awaiting_moderation | `Awaiting_confirmation ] * string * string) list ;
  pending_moderator_subscriptions : ([ `Awaiting_moderation | `Awaiting_confirmation ] * string * string) list ;
  subscription_moderated : bool ;
  subscribers : string list ;
  bounces : (int * int * string) list ;
  moderator_bounces : (int * int * string) list ;
  moderated : bool ;
  headers : string list ;
  footer : string option ;
  welcome : string option ;
  goodbye : string option ;
  pending_mails : (string * string) list ;
  who_can_post : [ `Moderators | `Subscribers | `Public ] ;
  who_is_moderated : [ `Moderators | `Subscribers | `Public ] ;
  counter : int ;
}

(* The only event we have is an incoming mail, to which we react (or not) *)
let subsc_req name queue from =
  let id = gen_random () in
  let queue = List.filter (fun (_, _, from') -> not (String.equal from from')) queue in
  let queue = (`Awaiting_confirmation, id, from) :: queue in
  let mail = name ^ "-subscribe-" ^ id, [ from ], "Please reply to this mail to confirm your subscription" in
  queue, mail

let subscription_request t from =
  let pending_subscriptions, mail = subsc_req t.name t.pending_subscriptions from in
  { t with pending_subscriptions }, Some mail

let moderator_subscription_request t from =
  let pending_moderator_subscriptions, mail = subsc_req (t.name ^ "-owners") t.pending_moderator_subscriptions from in
  { t with pending_moderator_subscriptions }, Some mail

let subscr_confirm name queue moderated moderators subscribers welcome from rcpt =
  let id = extract rcpt in
  match List.partition (function (`Awaiting_confirmation, id', from') -> String.equal id id' && String.equal from from' | _ -> false) queue with
  | [ _subscription ], rest ->
    if moderated then
      let id' = gen_random () in
      let mail = name ^ "-subscribe-" ^ id', moderators, Fmt.str "%s would like to be subscribed to this mailing list, please reply to this mail to confirm" from in
      let queue = (`Awaiting_moderation, id', from) :: rest in
      queue, None, Some mail
    else
      let subscribers = from :: subscribers in
      let mail = Option.map (fun txt -> name ^ "-owner", [ from ], txt) welcome in
      queue, Some subscribers, mail
  | _ ->
    match List.partition (function (`Awaiting_moderation, id', orig_from) -> String.equal id id' && List.mem from moderators | _ -> false) queue with
    | [ _subscription ], rest ->
      let subscribers = from :: subscribers in
      let mail = Option.map (fun txt -> name ^ "-owner", [ from ], txt) welcome in
      rest, Some subscribers, mail
    | _ ->
      (* ignore or should we send something out? *)
      queue, None, None

let subscription_confirmation t from rcpt =
  let pending_subscriptions, subscribers, mail =
    subscr_confirm t.name t.pending_subscriptions t.subscription_moderated t.moderators t.subscribers t.welcome from rcpt
  in
  (match subscribers with
   | None -> ()
   | Some _ -> Logs.info (fun m -> m "New subscription to the mailing list %s" t.name));
  let subscribers = Option.value ~default:t.subscribers subscribers in
  { t with pending_subscriptions ; subscribers }, mail

let moderator_subscription_confirmation t from rcpt =
  let pending_moderator_subscriptions, moderators, mail =
    subscr_confirm (t.name ^ "-owners") t.pending_moderator_subscriptions true t.moderators t.moderators (Some ("Welcome to the owners of " ^ t.name)) from rcpt
  in
  (match moderators with
   | None -> ()
   | Some _ -> Logs.info (fun m -> m "New subscription to the mailing list %s-owners" t.name));
  let moderators = Option.value ~default:t.moderators moderators in
  { t with pending_moderator_subscriptions ; moderators }, mail

let unsub name subscribers goodbye from =
  let subscribers' = List.filter (fun n -> not (String.equal n from)) subscribers in
  if List.length subscribers' < List.length subscribers then
    let mail = Option.map (fun txt -> name ^ "-owner", [from ], txt) goodbye in
    subscribers', mail
  else
    subscribers, None

let unsubscribe t from =
  let subscribers, mail = unsub t.name t.subscribers t.goodbye from in
  (if List.length subscribers < List.length t.subscribers then
     Logs.info (fun m -> m "Unsubscription from the mailing list %s" t.name));
  { t with subscribers }, mail

let moderator_unsubscribe t from =
  let moderators, mail = unsub (t.name ^ "-owners") t.moderators (Some ("Goodbye from the owners of " ^ t.name)) from in
  (if List.length moderators < List.length t.moderators then
     Logs.info (fun m -> m "Unsubscription from the mailing list %s-owners" t.name));
  { t with moderators }, mail

let bounce_logic name subscribers bounces from rcpt =
  let id, mail = extract_id_mail rcpt in
  if List.mem mail subscribers then
    let score =
      let (score, old_id, _) =
        Option.value ~default:(0, -1, mail)
          (List.find_opt (fun (_, _, f) -> String.equal mail f) bounces)
      in
      if old_id = -1 || old_id = pred id then
        score + 1
      else
        1
    in
    let b_without = List.filter (fun (_, _, f) -> not (String.equal mail f)) bounces in
    if score >= 5 then
      let subscribers, _email = unsub name subscribers None mail in
      b_without, subscribers
    else
      let bounces = (id, score, mail) :: b_without in
      bounces, subscribers
  else
    bounces, subscribers

let bounce t from rcpt =
  let bounces, subscribers =
    bounce_logic t.name t.subscribers t.bounces from rcpt
  in
  (if List.length subscribers < List.length t.subscribers then
     Logs.info (fun m -> m "Unsubscription from the mailing list %s (bounced)" t.name));
  { t with subscribers ; bounces }, None

let moderator_bounce t from rcpt =
  let moderator_bounces, moderators =
    bounce_logic (t.name ^ "-owners") t.moderators t.moderator_bounces from rcpt
  in
  (if List.length moderators < List.length t.moderators then
     Logs.info (fun m -> m "Unsubscription from the mailing list %s-owners (bounced)" t.name));
  { t with moderators ; moderator_bounces }, None

let forward_mail t mail =
  let from mail =
    let mail_at_equal = String.replace "@" "=" mail in
    t.name ^ "-return-" ^ string_of_int t.counter ^ "-" ^ mail
  in
  let email = t.headers ^ mail ^ Option.value ~default:"" t.footer in
  let mails = List.map (fun subscriber -> from subscriber, [ subscriber ], email) t.subscribers in
  { t with counter = t.counter + 1 }, mails

let moderate t from rcpt =
  if List.mem from t.moderators then
    let id = extract_id rcpt in
    match List.partition (fun (id', _mail) -> String.equal id id') t.pending_mails with
    | [ (_, mail) ], rest ->
      let t = { t with pending_mails = rest } in
      forward_mail t mail
    | _ -> t, []
  else
    t, []

let forward t from mail =
  let should_forward = match t.who_can_post with
    | `Moderators -> List.mem from t.moderators
    | `Subscribers -> List.mem from t.subscribers
    | `Public -> true
  in
  if should_forward then
    if t.moderated then
      let need_to_queue =
        match t.who_is_moderated with
        | `Public -> not (List.mem from t.subscribers)
        | `Subscribers -> not (List.mem from t.moderators)
        | `Moderators -> true
      in
      if need_to_queue then
        let id = gen_random () in
        let email = t.name ^ "-moderate-" ^ id, t.moderators, "Please reply to let this mail go through" in
        let pending_mails = (id, mail) :: t.pending_mails in
        { t with pending_mails }, [ email ]
      else
        forward_mail t mail
    else
      forward_mail t mail
  else
    t, []

(* We ultimately need to figure out from the incoming mail whether it is an
   administrative command (and we handle this after checking) or not. We may
   need to reply by sending some mails. *)
let incoming t email =
  let from = find_from email
  and rcpt = find_rcpt email
  in
  let opt_to_list (t, mail) = t, Option.to_list mail in
  match rcpt with
  | x when String.starts_with ~prefix:(t.name ^ "-subscribe-") x ->
    subscription_confirmation t from rcpt |> opt_to_list
  | x when String.starts_with ~prefix:(t.name ^ "-subscribe") x ->
    subscription_request t from |> opt_to_list
  | x when String.starts_with ~prefix:(t.name ^ "-owners-subscribe-") x ->
    moderator_subscription_confirmation t from rcpt |> opt_to_list
  | x when String.starts_with ~prefix:(t.name ^ "-owners-subscribe") x ->
    moderator_subscription_request t from |> opt_to_list
  | x when String.starts_with ~prefix:(t.name ^ "-unsubscribe") x ->
    unsubscribe t from |> opt_to_list
  | x when String.starts_with ~prefix:(t.name ^ "-owners-unsubscribe") x ->
    moderator_unsubscribe t from |> opt_to_list
  | x when String.starts_with ~prefix:(t.name ^ "-return-") x ->
    bounce t from rcpt |> opt_to_list
  | x when String.starts_with ~prefix:(t.name ^ "-owners-return-") x ->
    moderator_bounce t from rcpt |> opt_to_list
  | x when String.starts_with ~prefix:t.name x ->
    forward t from email
  | _ -> assert false

