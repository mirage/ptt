let src = Logs.Src.create "ptt.sendmail"

module Log = (val Logs.src_log src)

open Lwt.Infix

let ( <$> ) f g = fun x -> match g x with
  | Ok x -> f x | Error _ as err -> err

[@@@warning "-30"]

type recipients =
  { domain : [ `Ipaddr of Ipaddr.t | `Domain of [ `host ] Domain_name.t ]
  ; locals : [ `All | `Some of Emile.local list | `Postmaster ] }
and 'dns t =
  { stream : elt Lwt_stream.t
  ; push : elt push
  ; info : Ptt_common.info
  ; resolver : 'dns Ptt_common.resolver
  ; tls : Tls.Config.client
  ; pool : pool option }
and elt =
  { sender : Colombe.Reverse_path.t
  ; recipients : recipients
  ; data : string Lwt_stream.t
  ; policies : policy list
  ; id : Mrmime.MessageID.t }
and pool = { pool : 'a. (resource -> 'a Lwt.t) -> 'a Lwt.t }
and resource = bytes * bytes * (char, Bigarray.int8_unsigned_elt) Ke.Rke.t
and 'a push = 'a option -> unit
and policy = [ `Ignore ]

[@@@warning "+30"]

let warn_about_an_unreachable_mail_exchange ~domain ~mail_exchange msg =
  Log.warn @@ fun m -> m "Impossible to resolve %a, a mail exchange server for %a: %s"
    Domain_name.pp mail_exchange Domain_name.pp domain msg

(* recipients -> Colombe.Forward_path.t list *)
let recipients_to_forward_paths recipients =
  let open Colombe in
  let open Forward_path in
  let domain = match recipients.domain with
    | `Ipaddr (Ipaddr.V4 v4) -> Domain.IPv4 v4
    | `Ipaddr (Ipaddr.V6 v6) -> Domain.IPv6 v6
    | `Domain domain -> Domain.Domain (Domain_name.to_strings domain) in
  let local_to_forward_path local =
    let local = List.map (function `Atom x -> x | `String x -> x) local in
    Forward_path { Path.local= `Dot_string local; domain; rest= [] } in
  match recipients.locals with
  | `All -> [ Domain domain ]
  | `Some locals -> List.map local_to_forward_path locals
  | `Postmaster -> [ Postmaster ]

let guess_return_path stream =
  let open Mrmime in
  let decoder = Hd.decoder Field_name.Map.empty in
  let extract_return_path
    : type a. a Field.t -> a -> Colombe.Forward_path.t option Lwt.t
    = fun w v -> match w with
    | Unstructured ->
      let str = Unstructured.to_string v in
      begin match (Colombe_emile.to_forward_path <$> Emile.of_string) str with
      | Ok recipient -> Lwt.return_some recipient
      | Error _ -> Lwt.return_none end
    | _ -> Lwt.return_none in
  let rec go decoder = match Hd.decode decoder with
    | `End _ | `Malformed _ -> Lwt.return_none
    | `Field field ->
      let Field.Field (field_name, w, v) = Location.without_location field in
      if Field_name.equal field_name Field_name.return_path
      then extract_return_path w v
      else go decoder
    | `Await ->
      Lwt_stream.get stream >>= function
      | Some str -> Hd.src decoder str 0 (String.length str); go decoder
      | None -> Lwt.return_none in
  go decoder

module Make
  (Clock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
  (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow)
= struct
  module Sendmail = Sendmail_mirage.Make (Clock) (Stack.TCP) (Happy_eyeballs)

  let to_stream stream =
    let stream = Lwt_stream.map (fun str -> str, 0, String.length str) stream in
    let consumed = ref false in
    consumed, (fun () -> consumed := true; Lwt_stream.get stream)

  (* NOTE(dinosaure): to ensure that we are able to inject a fake DNS resolver,
     we must use an IP address as a destination to avoid the resolution mechanism
     of happy-eyeballs! *)

  let sendmail ?(last_option= false) he t ~ipaddr elt =
    let ( let* ) = Lwt.bind in
    let destination = Ipaddr.to_string ipaddr in
    let backup = Lwt_stream.clone elt.data in
    let consumed, stream = to_stream elt.data in
    let recipients = recipients_to_forward_paths elt.recipients in
    let* result = match t.pool with
      | Some { pool } ->
        pool @@ fun (encoder, decoder, queue) ->
          let encoder = Fun.const encoder in
          let decoder = Fun.const decoder in
          let queue = Fun.const queue in
          Sendmail.sendmail ~encoder ~decoder ~queue he ~destination
            ~cfg:t.tls ~domain:t.info.Ptt_common.domain elt.sender recipients stream
      | None ->
          Sendmail.sendmail he ~destination ~cfg:t.tls
            ~domain:t.info.Ptt_common.domain elt.sender recipients stream in
    if not last_option
    then match result, !consumed with
      | Ok (), _ -> Lwt.return `Ok
      | Error _, false -> Lwt.return `Retry
      | Error err, true ->
          let* forward_path = guess_return_path backup in
          Lwt.return (`Errored (forward_path, err))
    else match result with
      | Ok () -> Lwt.return `Ok
      | Error _ when List.exists ((=) `Ignore) elt.policies -> Lwt.return `Ok
      | Error err ->
          let* forward_path = guess_return_path backup in
          Lwt.return (`Errored (forward_path, err))

  let no_mail_exchange_service elt =
    let ( let* ) = Lwt.bind in
    let* forward_path = guess_return_path (Lwt_stream.clone elt.data) in
    match forward_path with
    | None ->
        let recipients = recipients_to_forward_paths elt.recipients in
        Log.err (fun m -> m "Impossible to send the email %a to @[<hov>%a@] \
          and impossible to find a return-path to notify the sender %a"
          Mrmime.MessageID.pp elt.id
          Fmt.(list ~sep:(any ",") Colombe.Forward_path.pp) recipients
          Colombe.Reverse_path.pp elt.sender);
        Lwt.return_unit
    | Some _forward_path -> assert false (* TODO *)

  let pp_error ppf = function
    | #Sendmail_with_starttls.error as err ->
      Sendmail_with_starttls.pp_error ppf err
    | `Msg msg -> Fmt.string ppf msg

  let error_while_sending_email elt (forward_path, err) =
    match forward_path with
    | None ->
        let recipients = recipients_to_forward_paths elt.recipients in
        Log.err (fun m -> m "Got an error while sending email %a to \
          @[<hov>%a@] and impossible to find a return-path to notify the 
          sender %a: %a" Mrmime.MessageID.pp elt.id
          Fmt.(list ~sep:(any ",") Colombe.Forward_path.pp) recipients
          Colombe.Reverse_path.pp elt.sender
          pp_error err);
        Lwt.return_unit
    | Some _forward_path -> assert false (* TODO *)

  let sendmail dns he t elt =
    let ( let* ) = Lwt.bind in
    let open Ptt_common in
    begin match elt.recipients.domain with
      | `Ipaddr ipaddr ->
        let domain = Ipaddr.to_domain_name ipaddr in
        Lwt.return_ok Mxs.(v ~preference:0 ~domain ipaddr)
      | `Domain domain ->
        let* r = t.resolver.getmxbyname dns domain in
        match r with
        | Ok mxs ->
            let resolve = (Fun.flip Lwt_list.fold_left_s [] )
              begin fun acc ({ Dns.Mx.mail_exchange; _ } as mx) ->
                let* r = t.resolver.gethostbyname dns mail_exchange in
                match r with
                | Ok ipaddr -> Lwt.return ((mx, ipaddr) :: acc)
                | Error (`Msg msg) ->
                  warn_about_an_unreachable_mail_exchange ~domain ~mail_exchange msg;
                  Lwt.return acc end in
            resolve (Dns.Rr_map.Mx_set.elements mxs) >|= Mxs.vs >|= Result.ok
        | Error _ as err -> Lwt.return err end >>= function
    | Error _ -> no_mail_exchange_service elt
    | Ok mxs ->
      if Mxs.is_empty mxs
      then no_mail_exchange_service elt
      else
        let mxs = Mxs.bindings mxs in
        let rec go = function
          | [] ->
              (* NOTE(dinosaure): we verified that [mxs] contains at least one
                 field and we catch up the case when we have the last element
                 of [mxs] which does not do the recursion. This case should
                 never occur. *)
              assert false
          | [ _mx, ipaddr ] ->
            let* result = sendmail ~last_option:true he t ~ipaddr elt in
            begin match result with
            | `Retry | `Ok -> Lwt.return_unit
            | `Errored value -> error_while_sending_email elt value end
          | (_mx, ipaddr) :: mxs ->
            let* result = sendmail he t ~ipaddr elt in
            match result with
            | `Ok -> Lwt.return_unit
            | `Retry -> go mxs
            | `Errored value -> error_while_sending_email elt value in
        go mxs

  let rec job dns he t =
    Lwt_stream.get t.stream >>= function
    | Some elt ->
      sendmail dns he t elt >>= fun () ->
      job dns he t
    | None -> Lwt.return_unit

  let v
    : resolver:_ -> ?pool:pool -> info:Ptt_common.info -> Tls.Config.client -> _
    = fun ~resolver ?pool ~info tls ->
    let stream, push = Lwt_stream.create () in
    { stream; push; info; resolver; tls; pool }, push
end
