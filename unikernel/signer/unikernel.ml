open Rresult
open Lwt.Infix

exception Invalid_certificate

let ( >>? ) = Lwt_result.bind

let ( $ ) f g = fun x -> match f x with Ok x -> g x | Error _ as err -> err
let ( <.> ) f g = fun x -> f (g x)
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt

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
    let doc = Arg.info ~doc:"nsupdate key" [ "dns-key" ] in
    let key = Arg.conv ~docv:"HOST:HASH:DATA" Dns.Dnskey.(name_key_of_string, pp_name_key) in
    Arg.(required & opt (some key) None doc)

  let dns_server =
    let doc = Arg.info ~doc:"dns server IP" [ "dns-server" ] in
    Arg.(required & opt (some Mirage_runtime_network.Arg.ip_address) None doc)

  let dns_port =
    let doc = Arg.info ~doc:"dns server port" [ "dns-port" ] in
    Arg.(value & opt int 53 doc)

  let destination =
    let doc = Arg.info ~doc:"Next SMTP server IP" [ "destination" ] in
    Arg.(required & opt (some Mirage_runtime_network.Arg.ip_address) None doc)

  let fields =
    let doc = Arg.info [ "fields" ] ~doc:"List of fields to sign" in
    let field = Arg.conv Mrmime.Field_name.(of_string, pp) in
    Arg.(value & opt (some (list ~sep:',' field)) None doc)

  let selector =
    let doc = Arg.info [ "selector" ] ~doc:"The DKIM selector." in
    let part = Arg.conv Domain_name.(of_string, pp) in
    Arg.(required & opt (some part) None doc)

  let timestamp =
    let doc = Arg.info [ "timestamp" ] ~doc:"The epoch time that the private key was created." in
    Arg.(value & opt (some int64) None doc)

  let expiration =
    let doc = Arg.info [ "expiration" ] ~doc:"The signature expiration (epoch time)." in
    Arg.(value & opt (some int64) None doc)

  let seed =
    let doc = Arg.info [ "seed" ] ~doc:"The seed (in base64) of the private RSA key." in
    let parser str = Base64.decode str in
    let pp ppf str = Fmt.string ppf (Base64.encode_exn str) in
    let b64 = Arg.conv (parser, pp) in
    Arg.(required & opt (some b64) None doc)

  type t =
    { domain : [ `host ] Domain_name.t
    ; postmaster : Emile.mailbox
    ; destination : Ipaddr.t
    ; dns_key : [ `raw ] Domain_name.t * Dns.Dnskey.t
    ; dns_server : Ipaddr.t
    ; dns_port : int
    ; fields : Mrmime.Field_name.t list option
    ; selector : [ `raw ] Domain_name.t
    ; timestamp : int64 option
    ; expiration : int64 option
    ; seed : string }

  let v domain postmaster destination
    dns_key dns_server dns_port
    fields selector timestamp expiration seed =
    { domain; postmaster; destination; dns_key; dns_server; dns_port; fields; selector; timestamp; expiration; seed }

  let setup =
    Term.(const v $ domain $ postmaster $ destination
    $ dns_key $ dns_server $ dns_port
    $ fields $ selector $ timestamp $ expiration $ seed)
end

module Make
  (Random : Mirage_random.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
  (Dns_client : Dns_client_mirage.S)
  (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow)
= struct
  (* XXX(dinosaure): this is a fake resolver which enforce the [signer] to
   * transmit **any** emails to only one and unique SMTP server. *)

  module DKIM = Dkim_mirage.Make (Pclock) (Dns_client)
  module Nss = Ca_certs_nss.Make (Pclock)

  let private_rsa_key_from_seed seed =
    let g = Mirage_crypto_rng.(create ~seed (module Fortuna)) in
    Mirage_crypto_pk.Rsa.generate ~g ~bits:2048 ()

  let ns_check dkim value dns =
    DKIM.server dns dkim >>= function
    | Ok value'->
      Logs.info (fun m -> m "The DNS server already has a DKIM public key: %a (expected: %a)."
        Dkim.pp_server value' Dkim.pp_server value) ;
      if Dkim.equal_server value value'
      then Lwt.return `Already_registered else Lwt.return `Must_be_updated
    | Error _ ->
      Logs.info (fun m -> m "The DNS server does not have the DKIM public key.") ;
      Lwt.return `Not_found

  module DNS = Dns_mirage.Make (Stack)

  let ns_update (key_name, dns_key) dkim value stack dns { K.dns_server; dns_port; _ } =
    ns_check dkim value dns >>= function
    | `Already_registered -> Lwt.return_ok ()
    | `Must_be_updated | `Not_found ->
      let dkim_domain = R.failwith_error_msg (Dkim.domain_name dkim) in
      let key_name, key_zone, dns_key =
        match Domain_name.find_label key_name (function "_update" -> true | _ -> false) with
        | None -> Fmt.failwith "The given DNS key is not an update key"
        | Some idx ->
          let amount = succ idx in
          let zone = Domain_name.(host_exn (drop_label_exn ~amount key_name)) in
          key_name, zone, dns_key in
      Stack.TCP.create_connection (Stack.tcp stack) (dns_server, dns_port)
      >|= R.reword_error (R.msgf "%a" Stack.TCP.pp_error) >>? fun flow ->
      let v = Dns.Packet.Update.Add
        Dns.Rr_map.(B (Txt, (3600l, Txt_set.singleton (Dkim.server_to_string value)))) in
      let packet =
        let header = (Randomconv.int16 Random.generate, Dns.Packet.Flags.empty) in
        let zone = Dns.Packet.Question.create key_zone Dns.Rr_map.Soa in
        Dns.Packet.create header zone (`Update Domain_name.Map.(empty, singleton dkim_domain [ v ])) in
      Lwt.bind
        begin
          Dns_tsig.encode_and_sign ~proto:`Tcp packet (Ptime.v (Pclock.now_d_ps ()))
          dns_key key_name |> R.reword_error (R.msgf "%a" Dns_tsig.pp_s) |> Lwt.return >>? fun (data, mac) ->
          DNS.send_tcp flow (Cstruct.of_string data) 
          >|= R.reword_error (fun _ -> R.msgf "Impossible to send a DNS packet to %a:%d"
            Ipaddr.pp dns_server dns_port) >>? fun () -> DNS.read_tcp (DNS.of_flow flow)
          >|= R.reword_error (fun _ -> R.msgf "Impossible to read a DNS packet from %a:%d"
            Ipaddr.pp dns_server dns_port) >>? fun data ->
          let data = Cstruct.to_string data in
          Dns_tsig.decode_and_verify (Ptime.v (Pclock.now_d_ps ())) dns_key key_name ~mac data
          |> R.reword_error (R.msgf "%a" Dns_tsig.pp_e)
          |> Lwt.return >>? fun (packet', _tsig, _mac) ->
          match Dns.Packet.reply_matches_request ~request:packet packet' with
          | Ok _ -> Lwt.return_ok ()
          | Error _ -> assert false
        end @@ fun res -> Stack.TCP.close flow >>= fun () -> Lwt.return res

  let start _random _time _mclock _pclock stack dns he
    ({ K.domain; postmaster; destination; dns_key; fields; selector; seed; timestamp; expiration; _ } as cfg) =
    let dkim = Dkim.v
      ~version:1 ?fields ~selector
      ~algorithm:`RSA
      ~query:(`DNS (`TXT))
      ?timestamp
      ?expiration
      (Domain_name.raw domain) in
    let private_key = private_rsa_key_from_seed seed in
    let value = Dkim.server_of_dkim ~key:private_key dkim in
    let authenticator = R.failwith_error_msg (Nss.authenticator ()) in
    let tls = R.failwith_error_msg (Tls.Config.client ~authenticator ()) in
    ns_update dns_key dkim value stack dns cfg >|= R.failwith_error_msg >>= fun () ->
    let ip = Stack.ip stack in
    let ipaddr = List.hd (Stack.IP.configured_ips ip) in
    let ipaddr = Ipaddr.Prefix.address ipaddr in
    let locals = Ptt_map.empty ~postmaster in
    let module Fake_dns = Ptt_fake_dns.Make (struct let ipaddr = destination end) in
    let module Nec = Nec.Make (Time) (Mclock) (Pclock) (Stack) (Fake_dns) (Happy_eyeballs) in
    let info =
      { Ptt_common.domain= Colombe.Domain.Domain (Domain_name.to_strings domain)
      ; ipaddr
      ; tls= None
      ; zone= Mrmime.Date.Zone.GMT
      ; size= 10_000_000L } in
    Fake_dns.connect () >>= fun dns ->
    Nec.job ~locals ~port:25 ~tls ~info (Stack.tcp stack) dns he (private_key, dkim)
end
