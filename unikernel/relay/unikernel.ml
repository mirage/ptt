open Rresult
open Lwt.Infix

let local_of_string str =
  match Angstrom.parse_string ~consume:All Emile.Parser.local_part str with
  | Ok v -> Ok v | Error _ -> Error (R.msgf "Invalid local-part: %S" str)

let ( $ ) f g = fun x -> match f x with Ok x -> g x | Error _ as err -> err
let ( <.> ) f g = fun x -> f (g x)
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt

module K = struct
  open Cmdliner

  let remote =
    let doc = Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
    Arg.(required & opt (some string) None doc)

  let domain =
    let doc = Arg.info ~doc:"SMTP domain-name." [ "domain" ] in
    let domain_name = Arg.conv (Domain_name.(of_string $ host), Domain_name.pp) in
    Arg.(required & opt (some domain_name) None doc)

  let postmaster =
    let doc = Arg.info ~doc:"The postmaster of the SMTP service." [ "postmaster" ] in
    let mailbox = Arg.conv (Result.map_error (msgf "%a" Emile.pp_error) <.> Emile.of_string, Emile.pp_mailbox) in
    Arg.(required & opt (some mailbox) None doc)

  let nameservers =
    let doc = Arg.info ~doc:"DNS nameservers." [ "nameserver" ] in
    Arg.(value & opt_all string [] doc)

  type t =
    { remote : string
    ; domain : [ `host ] Domain_name.t
    ; postmaster : Emile.mailbox }

  let v remote domain postmaster =
    { remote; domain; postmaster }

  let setup = Term.(const v $ remote $ domain $ postmaster)
end

module Make
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
  (DNS : Dns_client_mirage.S with type 'a Transport.io = 'a Lwt.t)
  (_ : sig end)
= struct
  module Store = Git_kv.Make (Pclock)

  module Resolver = struct
    type t = DNS.t
    type +'a io = 'a Lwt.t

    (* XXX(dinosaure): it seems that Gmail does not like IPv6... A solution
       will be to aggregate IPv4 and IPv6 from the given domain and return
       both to test both... *)
    let gethostbyname dns domain_name =
      DNS.gethostbyname dns domain_name >|= function
      | Ok ipv4 -> Ok (Ipaddr.V4 ipv4)
      | Error _ as err -> err

    let getmxbyname dns domain_name =
      DNS.getaddrinfo dns Dns.Rr_map.Mx domain_name >>= function
      | Ok (_ttl, mxs) -> Lwt.return_ok mxs
      | Error _ as err -> Lwt.return err

    let extension _dns ldh value =
      Lwt.return_error (R.msgf "[%s:%s] is not supported" ldh value)
  end

  module Mti_gf = Mti_gf.Make (Time) (Mclock) (Pclock) (Resolver) (Stack)
  module Nss = Ca_certs_nss.Make (Pclock)

  let relay_map relay_map ctx remote =
    Git_kv.connect ctx remote >>= fun t ->
    Store.list t Mirage_kv.Key.empty >>= function
    | Error err ->
      Logs.warn (fun m -> m "Got an error when we tried to list values from \
                             Git: %a."
        Store.pp_error err);
      Lwt.return relay_map (* FIXME(dinosaure): it seems that we got an error in any situation. *)
    | Ok values ->
      let f acc = function
        | (_, `Dictionary) -> Lwt.return acc
        | (name, `Value) ->
          Store.get t name >>= function
          | Error err ->
            Logs.warn (fun m -> m "Got an error when we tried to get data \
                                   from %a: %a"
              Mirage_kv.Key.pp name Store.pp_error err);
            Lwt.return acc
          | Ok str ->
            match Ptt_value.of_string_json str, local_of_string (Mirage_kv.Key.basename name) with
            | Ok { Ptt_value.targets; _ }, Ok local ->
              let acc = List.fold_left (fun acc x ->
                Ptt.Relay_map.add ~local x acc) acc targets in
              Lwt.return acc
            | _, Error (`Msg _) ->
              Logs.warn (fun m -> m "Invalid local-part: %a" Mirage_kv.Key.pp name);
              Lwt.return acc
            | Error (`Msg err), _ ->
              Logs.warn (fun m -> m "Invalid value for %a: %s" Mirage_kv.Key.pp name err);
              Lwt.return acc in
      Lwt_list.fold_left_s f relay_map values

  let start _time _mclock _pclock stack dns ctx { K.remote; domain; postmaster; } =
    let authenticator = R.failwith_error_msg (Nss.authenticator ()) in
    let tls = Rresult.R.failwith_error_msg (Tls.Config.client ~authenticator ()) in
    relay_map (Ptt.Relay_map.empty ~postmaster ~domain) ctx remote
    >>= fun locals ->
    let ip = Stack.ip stack in
    let ipaddr = List.hd (Stack.IP.configured_ips ip) in
    let ipaddr = Ipaddr.Prefix.address ipaddr in
    Mti_gf.fiber ~port:25 ~locals ~tls (Stack.tcp stack) dns
      { Ptt.Logic.domain
      ; ipaddr
      ; tls= None
      ; zone= Mrmime.Date.Zone.GMT
      ; size= 10_000_000L (* 10M *) }
end
