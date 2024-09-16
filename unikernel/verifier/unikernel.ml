open Rresult
open Lwt.Infix

let ( $ ) f g = fun x -> match f x with Ok x -> g x | Error _ as err -> err
let ( <.> ) f g = fun x -> f (g x)
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt

exception Invalid_certificate

let ( >>? ) = Lwt_result.bind

module K = struct
  open Cmdliner

  let domain =
    let doc = Arg.info ~doc:"SMTP domain-name." [ "domain" ] in
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

  let key_seed =
    let doc = Arg.info ~doc:"certificate key seed" ["key-seed"] in
    Arg.(required & opt (some string) None doc)

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
    { domain : [ `host ] Domain_name.t
    ; postmaster : Emile.mailbox
    ; destination : Ipaddr.t
    ; dns_key : [ `raw ] Domain_name.t * Dns.Dnskey.t
    ; dns_server : Ipaddr.t
    ; key_seed : string }

  let v domain postmaster destination dns_key dns_server key_seed =
    { domain; postmaster; destination; dns_key; dns_server; key_seed }

  let setup = Term.(const v $ domain $ postmaster $ destination $ dns_key $ dns_server $ key_seed)
end

module Make
  (Random : Mirage_crypto_rng_mirage.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
  (DNS : Dns_client_mirage.S with type 'a Transport.io = 'a Lwt.t)
= struct
  (* XXX(dinosaure): this is a fake resolver which enforce the [verifier] to
   * transmit **any** emails to only one and unique SMTP server. *)

  module Resolver = struct
    type t = Ipaddr.t
    type +'a io = 'a Lwt.t

    let gethostbyname ipaddr _domain_name = Lwt.return_ok ipaddr
    let getmxbyname _ipaddr mail_exchange = Lwt.return_ok (Dns.Rr_map.Mx_set.singleton { Dns.Mx.preference= 0; mail_exchange; })
    let extension ipaddr _ldh _value = Lwt.return_ok ipaddr
  end

  module Verifier = Hm.Make (Time) (Mclock) (Pclock) (Resolver) (Stack) (DNS)
  module Nss = Ca_certs_nss.Make (Pclock)
  module Certify = Dns_certify_mirage.Make (Random) (Pclock) (Time) (Stack)

  let retrieve_certs stack { K.domain; dns_key; dns_server; key_seed; _ } =
    Certify.retrieve_certificate stack ~dns_key:Fmt.(to_to_string Dns.Dnskey.pp_name_key dns_key)
      ~key_seed ~hostname:domain dns_server 53 >>= function
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

  let start _random _time _mclock _pclock stack dns ({ K.domain; postmaster; destination; _ } as cfg)=
    let authenticator = R.failwith_error_msg (Nss.authenticator ()) in
    let tls = R.failwith_error_msg (Tls.Config.client ~authenticator ()) in
    let ip = Stack.ip stack in
    let ipaddr = List.hd (Stack.IP.configured_ips ip) in
    let ipaddr = Ipaddr.Prefix.address ipaddr in
    let rec loop (certificates, expiration) =
      let stop = Lwt_switch.create () in
      let wait_and_stop () =
        Time.sleep_ns expiration >>= fun () ->
        retrieve_certs stack cfg >>= fun result ->
        Lwt_switch.turn_off stop >>= fun () ->
        Lwt.return result in
      let server () =
        Verifier.fiber ~port:25 ~tls (Stack.tcp stack) destination
          { Ptt.Logic.domain
          ; ipaddr
          ; tls= Some (R.failwith_error_msg (Tls.Config.server ~certificates ()))
          ; zone= Mrmime.Date.Zone.GMT
          ; size= 10_000_000L (* 10M *) }
          dns in
      Lwt.both (server ()) (wait_and_stop ()) >>= fun ((), result) ->
      loop result in
    retrieve_certs stack cfg >>= loop
end
