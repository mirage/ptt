let src = Logs.Src.create "ptt.common"

module Log = (val Logs.src_log src)

let ( <$> ) f g = fun x -> match g x with
  | Ok x -> f x | Error _ as err -> err

type mxs = Dns.Rr_map.Mx_set.t

type ('dns, 'a) getmxbyname =
     'dns
  -> [ `host ] Domain_name.t
  -> (mxs, [> `Msg of string ] as 'a) result Lwt.t

type ('dns, 'a) gethostbyname =
     'dns
  -> [ `host ] Domain_name.t
  -> (Ipaddr.t, [> `Msg of string ] as 'a) result Lwt.t

type 'dns resolver =
  { getmxbyname : 'a. ('dns, 'a) getmxbyname
  ; gethostbyname : 'a. ('dns, 'a) gethostbyname }

type info =
  { domain: Colombe.Domain.t
  ; ipaddr: Ipaddr.t
  ; tls: Tls.Config.server option
  ; zone: Mrmime.Date.Zone.t
  ; size: int64 }

module Mxs = Mxs

exception Extension_is_not_available of Colombe.Forward_path.t
exception Invalid_recipients of Colombe.Forward_path.t

module Set = Set.Make (struct type t = [ `host ] Domain_name.t let compare = Domain_name.compare end)

let recipients_are_reachable ~info dns resolver recipients =
  let domains =
    let open Colombe in
    List.fold_left (fun acc -> function
      | Forward_path.Postmaster -> acc (* NOTE(dinosaure): we ourselves should be available *)
      | Domain (Domain.IPv4 _ | Domain.IPv6 _)
      | Forward_path { Path.domain= (Domain.IPv4 _ | Domain.IPv6 _); _ } -> acc
      | Domain (Domain.Extension _)
      | Forward_path { Path.domain= Domain.Extension _; _ } as value ->
          raise (Extension_is_not_available value)
      | Domain (Domain.Domain domain)
      | Forward_path { Path.domain= Domain.Domain domain; _ } as value ->
          if Domain.equal (Domain.Domain domain) info.domain
          then acc (* NOTE(dinosaure): we ourselves should be available *)
          else match Domain_name.(host <$> of_strings) domain with
          | Ok domain_name -> Set.add domain_name acc
          | Error _ -> raise (Invalid_recipients value))
      Set.empty recipients
    |> Set.to_list in
  let ( let* ) = Lwt.bind in
  let mail_exchange_are_reachable { Dns.Mx.mail_exchange; _ } =
    let* result = resolver.gethostbyname dns mail_exchange in
    match result with
    | Ok _ -> Lwt.return true
    | Error _ -> Lwt.return false in
  let domain_are_reachable domain =
    let* result = resolver.getmxbyname dns domain in
    match result with
    | Ok mxs ->
        let lst = Dns.Rr_map.Mx_set.to_list mxs in
        let lst = List.sort Dns.Mx.compare lst in
        Lwt_list.exists_p mail_exchange_are_reachable lst
    | Error _ -> Lwt.return false in
  Lwt_list.for_all_p domain_are_reachable domains

let recipients_are_reachable ~info dns resolver recipients =
  Lwt.catch (fun () -> recipients_are_reachable ~info dns resolver recipients)
  @@ function
    | Extension_is_not_available recipient ->
        Log.warn (fun m -> m "Someone tries to send an email to an \
          extension: %a" Colombe.Forward_path.pp recipient);
        Lwt.return false
    | Invalid_recipients recipient ->
        Log.warn (fun m -> m "%a's destination is unreachable"
          Colombe.Forward_path.pp recipient);
        Lwt.return false
    | exn -> Lwt.reraise exn

let id_to_messageID ~info id =
  let local = [ `Atom (Fmt.str "%016Lx" id) ] in
  let open Colombe in
  let domain = match info.domain with
    | Domain.Domain ds -> `Domain ds
    | Domain.IPv4 v4 ->
      let domain_name = Ipaddr.V4.to_domain_name v4 in
      `Domain (Domain_name.to_strings domain_name)
    | Domain.IPv6 v6 ->
      let domain_name = Ipaddr.V6.to_domain_name v6 in
      `Domain (Domain_name.to_strings domain_name)
    | Domain.Extension _ -> failwith "The SMTP server domain must not be an extension" in
  local, domain
