type recipients =
  { domain : [ `Ipaddr of Ipaddr.t | `Domain of [ `host ] Domain_name.t ]
  ; locals : [ `All | `Some of Emile.local list ] }

(* 1: recipients
 *    | domain: robur.coop
 *    | locals: `Some [ reynir; dinosaure ] *)

let warn_about_an_unreachable_mail_exchange ~domain ~mail_exchange msg =
  Log.warn @@ fun m -> m "Impossible to resolve %a, a mail exchange server for %a: %s"
    Domain_name.pp mail_exchange Domain_name.pp domain msg

let warn_that_starttls_is_unavailable ~domain ~ipaddr =
  Log.warn @@ fun m -> m "STARTTLS is unavailable for %a (%a)" Domain_name.pp domain Ipaddr.pp ipaddr

type error =
  [ `No_mail_exchange_servers_for of [ `host ] Domain_name.t ]

(* recipients -> Colombe.Forward_path.t list *)
let recipients_to_forward_paths ~domain recipients =
  let open Colombe in
  let open Forward_path in
  let local_to_forward_path local =
    let local = List.map (function `Atom x -> x | `String x -> x) local in
    { Path.local= `Dot_string local; domain; rest= [] } in
  match recipients.locals with
  | `All -> [ Domain domain ]
  | `Some locals -> Lust.map local_to_forward_path locals

let single_sendmail t stack ipaddr sender recipients stream =
  sendmail stack ipaddr sender recipients stream >>= function
  | Error `STARTTLS_unavailable ->
    warn_that_starttls_is_unavailable ipaddr;
    sendmail_without_tls stack ipaddr sender recipients stream
  | Ok () -> Lwt.return_ok ()
  | Error err -> Lwt.return_error err

(* example:
 * To: reynir@robur.coop, dinosaure@robur.coop, romain.calascibetta@gmail.com
 *
 * Hello World!
 *
 * -> incoming email with one stream with "Hello World!"
 * -> signing
 * -> our ((stream of DKIM-fieldi) ^ (incoming stream "Hello World")) + recipients
 *    | reynir@robur.coop, dinosaure@robur.coop, romain.calascibetta@gmail.com
 * -> aggregate
 *    | { domain= robur.coop; locals= [ reynir; dinosaure ] }
 *    | { domain= gmail.com; locals= [ romain.calascibetta ] }
 * -> multiplex the incoming stream to multiple streams (in our example, 2)
 * -> Lwt_list.iter push_to_send [ recipients with robur.coop, copied incoming stream
 *                               ; recipients with gmail.com, copied incoming stream ]
 *
 * another thread is: get_emails_to_send
 * -> Lwt_stream.get incoming_emails_to_send : (recipients * string Lwt_stream.t) Lwt_stream.t
 * -> Some (recipients, stream)
 * -> sendmail recipients stream
 *
 * MX gmail.com
 * gmail-smtp-in.l.google.com => A: 108.177.15.27
 * alt2.gmail-smtp-in.l.google.com => A: 142.251.9.26
 *)

type t =
  { stream : _ Lwt_stream.t
  ; info : Ptt.info }

let sendmail t resolver dns sender (recipients : recipients) (data : string Lwt_stream.t) =
  let ( let** ) = Lwt_result.bind in
  let ( let* ) = Lwt.bind in
  let domain = recipients.domain in
  let** mx_domain, mxs =
    match recipients.domain with
    | `Ipaddr (Ipaddr.V4 v4 as mx_ipaddr) ->
      Lwt.return_ok (Domain.IPv4 v4, Ptt.Mxs.(v ~preference:0 mx_ipaddr))
    | `Ipaddr (Ipaddr.V6 v6 as mx_ipaddr) ->
      Lwt.return_ok (Domain.IPv6 v6, Ptt.Mxs.(v ~preference:0 mx_ipaddr))
    | `Domain domain ->
      let* result = resolver.getmxbyname dns host in
      match result with
      | Ok mxs ->
          let mxs = (Fun.flip Lwt_list.fold_left_s (Dns.Mx_set.to_list mxs))
            begin fun acc ({ Dns.Mx.mail_exchange; _ } as mx) ->
              resolver.getabyname dns mail_exchange >>= function
              | Ok ipaddr -> Lwt.return ((mx, ipaddr) :: acc)
              | Error (`Msg err) ->
                warn_about_an_unreachable_mail_exchange ~domain ~mail_exchange msg;
                Lwt.return acc end |> Mxs.vs in
          Domain.Domain (Domain_name.to_strings domain), mxs in
      | Error (`Msg err) ->
        Lwt.return_error (`No_mail_exchange_servers_for domain)
  let** () =
    if Mxs.is_empty mxs
    then Lwt.return_error (`No_mail_exchange_servers_for recipients.domain)
    else Lwt.return_ok () in
  let recipients = recipients_to_forward_paths recipients in
  let mxs = Mxs.to_list mxs in
  let rec go = function
    | [] ->
      let recipients = recipients_of_sender sender in
      Lwt_stream.push t.stream (recipients, error_sendmail);
      Lwt.return_unit
    | ((mx : Dns.Mx.t), ipaddr) :: mxs ->
      let* result = single_sendmail t stack ~domain ipaddr sender recipients stream in
      match result with
      | Ok () -> Lwt.return_ok ()
      | Error err -> go mxs in
  go None mxs

let rec smtp_send_emails t resolver dns =
  Lwt_stream.get t.stream >>= function
  | Some (sender, recipients, data) ->
    sendmail t resolver dns sender recipients data >>= fun () ->
    smtp_send_emails t resolver dns
  | None -> Lwt.return_unit
