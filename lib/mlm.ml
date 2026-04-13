(* The mailing list manager *)

(* TODO: the queues (pending_subscriptions, pending_moderator_subscriptions, pending_mails) need to be freed after some time (or explicitly). *)

(* All the automated mails as well need to have the return-path to be xx-return-... (for bounce processing) *)
type t = {
  name : string ;
  moderators : string list ;
  (* TODO use map (or LRU) for the pending_subscriptions and pending_moderator_subscriptions *)
  pending_subscriptions : ([ `Awaiting_moderation | `Awaiting_confirmation ] * string * string) list ;
  pending_moderator_subscriptions : ([ `Awaiting_moderation | `Awaiting_confirmation ] * string * string) list ;
  subscription_moderated : bool ;
  subscribers : string list ;
  (* TODO use maps for the bounces and moderator_bounces *)
  bounces : (int * int * string) list ;
  moderator_bounces : (int * int * string) list ;
  moderated : bool ;
  headers : string list ;
  footer : string option ;
  welcome : string option ;
  goodbye : string option ;
  (* TODO use map (or LRU) for the pending_mails *)
  pending_mails : (string * string) list ;
  who_can_post : [ `Moderators | `Subscribers | `Public ] ;
  who_is_moderated : [ `Moderators | `Subscribers | `Public ] ;
  counter : int ;
}

let make ?(subscription_moderated = true) ?(moderated = true) ?headers ?footer ?welcome ?goodbye ?(who_can_post = `Subscribers) ?(who_is_moderated = `Public) name moderators =
  let list_headers = [ "List-Id: " ^ name (* +domain *) ] in
  let headers = Option.to_list headers @ list_headers in
  {
    name ; moderators ;
    pending_subscriptions = [] ; pending_moderator_subscriptions = [] ;
    subscription_moderated ; subscribers = [] ;
    bounces = [] ; moderator_bounces = [] ;
    moderated ; headers ; footer ;
    welcome ; goodbye ; pending_mails = [] ;
    who_can_post ; who_is_moderated ; counter = 0
  }

(* TODO actually initialize with something random *)
let random_state = Random.State.make_self_init ()

let gen_random () = Uuidm.v4_gen random_state () |> Uuidm.to_string

let template_subscription_confirmation =
  "Please reply to this mail to confirm your subscription"

let accept_deny prefix =
  let id = gen_random () in
  id, prefix ^ "-accept-" ^ id, prefix ^ "-reject-" ^ id

let template_subscription_moderation email_address accept deny =
  Fmt.str "%S would like to be subscribed to this mailing list. If you accept, please send an empty mail to <%s> (you can reply to this email). If you reject, please send an empty mail to <%s>." email_address accept deny

let template_moderation email_address subject accept deny =
  Fmt.str "A mail from %S with the subject %S was received. Please send an empty mail to <%s> to distribute this mail (you can reply to this email). If you want to reject, please send an empty mail to <%s>."
    email_address subject accept deny

let subsc_req name queue from =
  let id = gen_random () in
  let queue = List.filter (fun (_, _, from') -> not (String.equal from from')) queue in
  let queue = (`Awaiting_confirmation, id, from) :: queue in
  let mail = name ^ "-subscribe-" ^ id, [ from ], template_subscription_confirmation in
  queue, mail

let subscription_request t from =
  let pending_subscriptions, mail = subsc_req t.name t.pending_subscriptions from in
  { t with pending_subscriptions }, Some mail

let moderator_subscription_request t from =
  let pending_moderator_subscriptions, mail = subsc_req (t.name ^ "-owners") t.pending_moderator_subscriptions from in
  { t with pending_moderator_subscriptions }, Some mail

let subscr_confirm name queue moderated moderators subscribers welcome from id =
  match List.partition (function (`Awaiting_confirmation, id', from') -> String.equal id id' && String.equal from from' | _ -> false) queue with
  | [ _subscription ], rest ->
    if moderated then
      let id', accept, deny = accept_deny (name ^ "-subscribe") in
      let mail = accept, moderators, template_subscription_moderation from accept deny in
      let queue = (`Awaiting_moderation, id', from) :: rest in
      queue, None, Some mail
    else
      let subscribers = from :: subscribers in
      let mail = Option.map (fun txt -> name ^ "-owners", [ from ], txt) welcome in
      queue, Some subscribers, mail
  | _ ->
    match List.partition (function (`Awaiting_moderation, id', _orig_from) -> String.equal id id' && List.mem from moderators | _ -> false) queue with
    | [ _subscription ], rest ->
      let subscribers = from :: subscribers in
      let mail = Option.map (fun txt -> name ^ "-owners", [ from ], txt) welcome in
      rest, Some subscribers, mail
    | _ ->
      (* ignore or should we send something out? *)
      queue, None, None

let reject_subscr queue moderators from id =
  match List.partition (function (`Awaiting_moderation, id', _orig_from) -> String.equal id id' && List.mem from moderators | _ -> false) queue with
  | [ _subscription ], rest ->
    (* should we send something out? *)
    rest
  | _ ->
    (* ignore or should we send something out? *)
    queue

let subscription_confirmation t from id =
  let pending_subscriptions, subscribers, mail =
    subscr_confirm t.name t.pending_subscriptions t.subscription_moderated t.moderators t.subscribers t.welcome from id
  in
  (match subscribers with
   | None -> ()
   | Some _ -> Logs.info (fun m -> m "New subscription to the mailing list %s" t.name));
  let subscribers = Option.value ~default:t.subscribers subscribers in
  { t with pending_subscriptions ; subscribers }, mail

let reject_subscription_confirmation t from id =
  let pending_subscriptions =
    reject_subscr t.pending_subscriptions t.moderators from id
  in
  { t with pending_subscriptions }, None

let moderator_subscription_confirmation t from id =
  let pending_moderator_subscriptions, moderators, mail =
    subscr_confirm (t.name ^ "-owners") t.pending_moderator_subscriptions true t.moderators t.moderators (Some ("Welcome to the owners of " ^ t.name)) from id
  in
  (match moderators with
   | None -> ()
   | Some _ -> Logs.info (fun m -> m "New subscription to the mailing list %s-owners" t.name));
  let moderators = Option.value ~default:t.moderators moderators in
  { t with pending_moderator_subscriptions ; moderators }, mail

let reject_moderator_subscription_confirmation t from id =
  let pending_moderator_subscriptions =
    reject_subscr t.pending_moderator_subscriptions t.moderators from id
  in
  { t with pending_moderator_subscriptions }, None

let unsub name subscribers goodbye from =
  let subscribers' = List.filter (fun n -> not (String.equal n from)) subscribers in
  if List.length subscribers' < List.length subscribers then
    let mail = Option.map (fun txt -> name ^ "-owners", [from ], txt) goodbye in
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

let extract_id_mail address =
  let mail_equal_is_at address =
    String.concat "@" (String.split_on_char '=' address)
  in
  match String.split_on_char '-' address with
  | id :: mail ->
    (match int_of_string_opt id with
     | Some id -> Ok (id, mail_equal_is_at (String.concat "-" mail))
     | None -> Error "couldn't find the id")
  | _ -> Error "couldn't extract id and mail address"

let bounce_logic name subscribers bounces rcpt =
  match extract_id_mail rcpt with
  | Ok (id, mail) ->
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
  | Error msg ->
    Logs.warn (fun m -> m "bounce %s: %s" rcpt msg);
    bounces, subscribers

let bounce t rcpt =
  let bounces, subscribers =
    bounce_logic t.name t.subscribers t.bounces rcpt
  in
  (if List.length subscribers < List.length t.subscribers then
     Logs.info (fun m -> m "Unsubscription from the mailing list %s (bounced)" t.name));
  { t with subscribers ; bounces }, None

let moderator_bounce t rcpt =
  let moderator_bounces, moderators =
    bounce_logic (t.name ^ "-owners") t.moderators t.moderator_bounces rcpt
  in
  (if List.length moderators < List.length t.moderators then
     Logs.info (fun m -> m "Unsubscription from the mailing list %s-owners (bounced)" t.name));
  { t with moderators ; moderator_bounces }, None

let mail_at_is_equal address = String.concat "=" (String.split_on_char '@' address)

let forward_mail t mail =
  let from mail =
    t.name ^ "-return-" ^ string_of_int t.counter ^ "-" ^ mail_at_is_equal mail
  in
  let email = String.concat "\n" t.headers ^ mail ^ Option.value ~default:"" t.footer in
  let mails = List.map (fun subscriber -> from subscriber, [ subscriber ], email) t.subscribers in
  { t with counter = t.counter + 1 }, mails

let moderate_accept t from id =
  if List.mem from t.moderators then
    match List.partition (fun (id', _mail) -> String.equal id id') t.pending_mails with
    | [ (_, mail) ], rest ->
      let t = { t with pending_mails = rest } in
      forward_mail t mail
    | _ -> t, []
  else
    t, []

let moderate_reject t from id =
  if List.mem from t.moderators then
    match List.partition (fun (id', _mail) -> String.equal id id') t.pending_mails with
    | [ _ ], rest ->
      let t = { t with pending_mails = rest } in
      t, []
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
        let id, accept, deny = accept_deny (t.name ^ "-moderate") in
        let subject = "subject" (* find_subject mail *) in
        let email = accept, t.moderators, template_moderation from subject accept deny in
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
let incoming t key stream =
  let ( let* ) = Result.bind in
  let from, _ = Ptt.Msgd.from key
  and recipients = Ptt.Msgd.recipients key
  in
  let* from =
    match from with
    | None ->
      (Logs.warn (fun m -> m "from is None");
       Error ())
    | Some p ->
      let local = match p.local with `String s -> s | `Dot_string xs -> String.concat "." xs in
      let domain = Colombe.Domain.to_string p.domain in
      Ok (local ^ "@" ^ domain)
  in
  let* rcpt = match recipients with
    | [ (Colombe.Forward_path.Forward_path p, _) ] ->
      (* should ensure that domain is good *)
      let local_part = match p.local with
        | `String s -> s
        | `Dot_string xs -> String.concat "." xs
      in
      Ok local_part
    | [ (fp, _) ] ->
      Logs.warn (fun m -> m "expected a forward path recipient, got %a"
                    Colombe.Forward_path.pp fp);
      Error ()
    | xs ->
      Logs.warn (fun m -> m "expected a single recipient: %a"
                    Fmt.(list ~sep:(any ", ") Colombe.Forward_path.pp)
                    (List.map fst xs));
      Error ()
  in
  let* leftovers =
    let rcpt_parts = String.split_on_char '-' rcpt in
    let name_parts = String.split_on_char '-' t.name in
    let rec parts a b = match a, b with
      | [], xs -> Ok xs
      | a::atl, b::btl ->
        if String.equal a b then
          parts atl btl
        else
          (Logs.warn (fun m -> m "rcpt %s doesn't match mailing list name %s" rcpt t.name);
           Error ())
      | _, [] ->
        Logs.warn (fun m -> m "rcpt %s does not match the mailing list name %s" rcpt t.name);
        Error ()
    in
    parts name_parts rcpt_parts
  in
  let opt_to_list (t, mail) = t, Option.to_list mail in
  let t, to_send =
    match leftovers with
    | "subscribe" :: "accept" :: id ->
      let id = String.concat "-" id in
      subscription_confirmation t from id |> opt_to_list
    | "subscribe" :: "reject" :: id ->
      let id = String.concat "-" id in
      reject_subscription_confirmation t from id |> opt_to_list
    | "subscribe" :: [] ->
      subscription_request t from |> opt_to_list
    | "subscribe" :: id ->
      let id = String.concat "-" id in
      subscription_confirmation t from id |> opt_to_list
    | "owners" :: "subscribe" :: "reject" :: id ->
      let id = String.concat "-" id in
      moderator_subscription_confirmation t from id |> opt_to_list
    | "owners" :: "subscribe" :: "accept" :: id ->
      let id = String.concat "-" id in
      reject_moderator_subscription_confirmation t from id |> opt_to_list
    | "owners" :: "subscribe" :: [] ->
      moderator_subscription_request t from |> opt_to_list
    | "owners" :: "subscribe" :: id ->
      let id = String.concat "-" id in
      moderator_subscription_confirmation t from id |> opt_to_list
    | "unsubscribe" :: [] ->
      unsubscribe t from |> opt_to_list
    | "owners" :: "unsubscribe" :: [] ->
      moderator_unsubscribe t from |> opt_to_list
    | "return" :: id ->
      let id = String.concat "-" id in
      bounce t id |> opt_to_list
    | "owners" :: "return" :: id ->
      let id = String.concat "-" id in
      moderator_bounce t id |> opt_to_list
    | "moderate" :: "accept" :: id ->
      let id = String.concat "-" id in
      moderate_accept t from id
    | "moderate" :: "reject" :: id ->
      let id = String.concat "-" id in
      moderate_reject t from id
    | [] -> forward t from stream
    (* TODO: should "owners" be distributed to the moderators? *)
    | x ->
      let id = String.concat "-" x in
      (* TODO: demote log level? send a bounce back? *)
      Logs.warn (fun m -> m "received mail to %s, which is not handled" id);
      t, []
  in
  Ok (t, to_send)
