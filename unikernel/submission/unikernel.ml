open Rresult
open Lwt.Infix

let ( <.> ) f g = fun x -> f (g x)
let ( $ ) f g = fun x -> match f x with Ok x -> g x | Error _ as err -> err
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt

let local_of_string str =
  match Angstrom.parse_string ~consume:All Colombe.Path.Decoder.local_part str with
  | Ok v -> Ok v | Error _ -> Error (R.msgf "Invalid local-part: %S" str)

module K = struct
  open Cmdliner

  let remote =
    let doc = Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
    Arg.(required & opt (some string) None doc)

  let domain =
    let doc = Arg.info ~doc:"SMTP domain-name." [ "domain" ] in
    let domain_name = Arg.conv Colombe.Domain.(of_string, pp) in
    Arg.(required & opt (some domain_name) None doc)

  let hostname =
    let doc = Arg.info ~doc:"Hostname of the SMTP submission server." [ "hostname" ] in
    let domain_name = Arg.conv (Domain_name.(of_string $ host), Domain_name.pp) in
    Arg.(required & opt (some domain_name) None doc)

  let postmaster =
    let doc = Arg.info ~doc:"The postmaster of the SMTP service." [ "postmaster" ] in
    let mailbox = Arg.conv (Result.map_error (msgf "%a" Emile.pp_error) <.> Emile.of_string, Emile.pp_mailbox) in
    Arg.(required & opt (some mailbox) None doc)

  let dns_key =
    let doc = Arg.info ~doc:"nsupdate key" ["dns-key"] in
    let key = Arg.conv ~docv:"HOST:HASH:DATA" Dns.Dnskey.(name_key_of_string, pp_name_key) in
    Arg.(required & opt (some key) None doc)

  let dns_server =
    let doc = Arg.info ~doc:"dns server IP" ["dns-server"] in
    Arg.(required & opt (some Mirage_runtime_network.Arg.ip_address) None doc)

  let destination =
    let doc = Arg.info ~doc:"Next SMTP server IP" ["destination"] in
    Arg.(required & opt (some Mirage_runtime_network.Arg.ip_address) None doc)

  let key_seed =
    let doc = Arg.info ~doc:"certificate key seed" ["key-seed"] in
    Arg.(required & opt (some string) None doc)

  let nameservers =
    let doc = Arg.info ~doc:"DNS nameservers." [ "nameserver" ] in
    Arg.(value & opt_all string [] doc)

  type t =
    { remote : string
    ; domain : Colombe.Domain.t
    ; hostname : [ `host ] Domain_name.t
    ; postmaster : Emile.mailbox
    ; destination : Ipaddr.t
    ; dns_key : [ `raw ] Domain_name.t * Dns.Dnskey.t
    ; dns_server : Ipaddr.t
    ; key_seed : string }

  let v remote domain hostname postmaster destination dns_key dns_server key_seed =
    { remote; domain; hostname; postmaster; destination; dns_key; dns_server; key_seed }

  let setup = Term.(const v $ remote $ domain $ hostname $ postmaster $ destination $ dns_key $ dns_server $ key_seed)
end

module Make
  (Random : Mirage_crypto_rng_mirage.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
  (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow)
  (_ : sig end)
= struct
  module Nss = Ca_certs_nss.Make (Pclock)
  module Certify = Dns_certify_mirage.Make (Random) (Pclock) (Time) (Stack)
  module Store = Git_kv.Make (Pclock)

  let authentication ctx remote =
    Git_kv.connect ctx remote >>= fun t ->
    Store.list t Mirage_kv.Key.empty >>= function
    | Error err ->
      Logs.warn (fun m -> m "Got an error when we tried to list values from \
                             Git: %a."
        Store.pp_error err);
      let authentication = Ptt.Authentication.v (fun _local _v -> Lwt.return false) in
      Lwt.return authentication
    | Ok values ->
      let tbl = Hashtbl.create 0x10 in
      let fill = function
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
            | Ok { Ptt_value.password; _ }, Ok local ->
              Hashtbl.add tbl local password;
              Lwt.return_unit
            | _, Error (`Msg _) ->
              Logs.warn (fun m -> m "Invalid local-part: %a" Mirage_kv.Key.pp name);
              Lwt.return_unit
            | Error (`Msg err), _ ->
              Logs.warn (fun m -> m "Invalid value for %a: %s" Mirage_kv.Key.pp name err);
              Lwt.return_unit in
      Lwt_list.iter_s fill values >>= fun () ->
      let authentication local v' = match Hashtbl.find tbl local with
        | v -> Lwt.return (Digestif.equal Digestif.BLAKE2B (Digestif.of_blake2b v) v')
        | exception _ -> Lwt.return false in
      let authentication = Ptt.Authentication.v authentication in
      Lwt.return authentication

  let retrieve_certs stack { K.hostname; dns_key; dns_server; key_seed; _ }=
    Certify.retrieve_certificate stack ~dns_key:Fmt.(to_to_string Dns.Dnskey.pp_name_key dns_key)
      ~key_seed ~hostname dns_server 53 >>= function
    | Error (`Msg err) -> failwith err
    | Ok certificates ->
      let now = Ptime.v (Pclock.now_d_ps ()) in
      let diffs =
        List.map (function (s :: _, _) -> s | _ -> assert false) [ certificates ]
        |> List.map X509.Certificate.validity
        |> List.map snd
        |> List.map (fun exp -> Ptime.diff exp now) in
      let next_expire = Ptime.Span.to_d_ps (List.hd (List.sort Ptime.Span.compare diffs)) in
      let next_expire = fst next_expire in
      let seven_days_before_expire = max (Duration.of_hour 1)
        (Duration.of_day (max 0 (next_expire - 7))) in
      Lwt.return (`Single certificates, seven_days_before_expire)

  let start _random _time _mclock _pclock stack he ctx ({ K.remote; domain; postmaster; destination; _ } as cfg) =
    let authenticator = R.failwith_error_msg (Nss.authenticator ()) in
    let tls = R.failwith_error_msg (Tls.Config.client ~authenticator ()) in
    let ip = Stack.ip stack in
    let ipaddr = List.hd (Stack.IP.configured_ips ip) in
    let ipaddr = Ipaddr.Prefix.address ipaddr in
    let locals = Ptt_map.empty ~postmaster in
    authentication ctx remote >>= fun authentication ->
    let module Fake_dns = Ptt_fake_dns.Make (struct let ipaddr = destination end) in
    let module Lipap = Lipap.Make (Time) (Mclock) (Pclock) (Stack) (Fake_dns) (Happy_eyeballs) in
    Fake_dns.connect () >>= fun dns ->
    let rec loop (certificates, expiration) =
      let info =
        { Ptt_common.domain
        ; ipaddr
        ; tls= Some (R.failwith_error_msg (Tls.Config.server ~certificates ()))
        ; zone= Mrmime.Date.Zone.GMT
        ; size= 10_000_000L (* 10M *) } in
      let stop = Lwt_switch.create () in
      let wait_and_stop () =
        Time.sleep_ns expiration >>= fun () ->
        retrieve_certs stack cfg >>= fun result ->
        Lwt_switch.turn_off stop >>= fun () ->
        Lwt.return result in
      let server () =
        Lipap.job ~locals ~port:465 ~tls ~info None Digestif.BLAKE2B (Stack.tcp stack)
          dns he authentication [ Ptt.Mechanism.PLAIN ] in
      Lwt.both (server ()) (wait_and_stop ()) >>= fun ((), result) ->
      loop result in
    retrieve_certs stack cfg >>= loop
end
