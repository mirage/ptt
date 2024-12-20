open Rresult
open Lwt.Infix

let local_of_string str =
  match Angstrom.parse_string ~consume:All Colombe.Path.Decoder.local_part str with
  | Ok v -> Ok v | Error _ -> Error (R.msgf "Invalid local-part: %S" str)

let ( <.> ) f g = fun x -> f (g x)
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt

module K = struct
  open Cmdliner

  let remote =
    let doc = Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
    Arg.(required & opt (some string) None doc)

  let domain =
    let doc = Arg.info ~doc:"SMTP domain-name." [ "domain" ] in
    let domain_name = Arg.conv Colombe.Domain.(of_string, pp) in
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
    ; domain : Colombe.Domain.t
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
  (Dns_client : Dns_client_mirage.S)
  (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow)
  (_ : sig end)
= struct
  module Store = Git_kv.Make (Pclock)

  module Mti_gf = Mti_gf.Make (Time) (Mclock) (Pclock) (Stack) (Dns_client) (Happy_eyeballs)
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
      let f = function
        | (_, `Dictionary) -> Lwt.return_unit
        | (name, `Value) ->
          Store.get t name >>= function
          | Error err ->
            Logs.warn (fun m -> m "Got an error when we tried to get data \
                                   from %a: %a"
              Mirage_kv.Key.pp name Store.pp_error err);
            Lwt.return_unit
          | Ok str ->
            match Ptt_value.of_string_json str, local_of_string (Mirage_kv.Key.basename name) with
            | Ok { Ptt_value.targets; _ }, Ok local ->
              List.iter (fun x -> Ptt_map.add ~local x relay_map) targets;
              Lwt.return_unit
            | _, Error (`Msg _) ->
              Logs.warn (fun m -> m "Invalid local-part: %a" Mirage_kv.Key.pp name);
              Lwt.return_unit
            | Error (`Msg err), _ ->
              Logs.warn (fun m -> m "Invalid value for %a: %s" Mirage_kv.Key.pp name err);
              Lwt.return_unit in
      Lwt_list.iter_p f values >>= fun () ->
      Lwt.return relay_map

  let start _time _mclock _pclock stack dns he ctx { K.remote; domain; postmaster; } =
    let authenticator = R.failwith_error_msg (Nss.authenticator ()) in
    let tls = Rresult.R.failwith_error_msg (Tls.Config.client ~authenticator ()) in
    relay_map (Ptt_map.empty ~postmaster) ctx remote
    >>= fun locals ->
    let ip = Stack.ip stack in
    let ipaddr = List.hd (Stack.IP.configured_ips ip) in
    let ipaddr = Ipaddr.Prefix.address ipaddr in
    let info =
      { Ptt_common.domain
      ; ipaddr
      ; tls= None
      ; zone= Mrmime.Date.Zone.GMT
      ; size= 10_000_000L (* 10M *) } in
    Mti_gf.job ~port:25 ~locals ~tls ~info (Stack.tcp stack) dns he
end
