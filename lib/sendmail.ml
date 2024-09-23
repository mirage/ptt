type recipients =
  { domain : [ `Ipaddr of Ipaddr.t | `Domain of [ `host ] Domain_name.t ]
  ; locals : [ `All | `Some of Emile.local list ] }

let warn_about_an_unreachable_mail_exchange ~domain ~mail_exchange msg =
  Log.warn @@ fun m -> m "Impossible to resolve %a, a mail exchange server for %a: %s"
    Domain_name.pp mail_exchange Domain_name.pp domain msg

type error =
  [ `No_mail_exchange_servers_for of [ `host ] Domain_name.t ]

let recipients_to_forward_paths ~domain recipients =
  let open Colombe in
  let open Forward_path in
  let local_to_forward_path local =
    let local = List.map (function `Atom x -> x | `String x -> x) local in
    { Path.local= `Dot_string local; domain; rest= [] } in
  match recipients.locals with
  | `All -> [ Domain domain ]
  | `Some locals -> Lust.map local_to_forward_path locals

let single_sendmail stack 

let sendmail ~pool ~info ~tls stack recipients data =
  let ( let** ) = Lwt_result.bind in
  let** mx_domain, mxs =
    match recipients.domain with
    | `Ipaddr (Ipaddr.V4 v4 as mx_ipaddr) ->
      Lwt.return_ok (Domain.IPv4 v4, Ptt.Mxs.(v ~preference:0 mx_ipaddr))
    | `Ipaddr (Ipaddr.V6 v6 as mx_ipaddr) ->
      Lwt.return_ok (Domain.IPv6 v6, Ptt.Mxs.(v ~preference:0 mx_ipaddr))
    | `Domain domain ->
      let* result = getmxbyname dns host in
      match result with
      | Ok mxs ->
          let mxs = (Fun.flip Lwt_list.fold_left_s (Dns.Mx_set.to_list mxs))
            begin fun acc ({ Dns.Mx.mail_exchange; _ } as mx) -> getabyname dns mail_exchange >>= function
            | Ok ipaddr -> Lwt.return ((mx, ipaddr) :: acc)
            | Error (`Msg err) ->
              warn_about_an_unreachable_mail_exchange ~domain ~mail_exchange msg;
              Lwt.return acc end |> Mxs.vs in
          Domain.Domain (Domain_name.to_strings domain), mxs in
      | Error (`Msg err) ->
        Lwt.return_error (`No_mail_exchange_servers_for domain)
