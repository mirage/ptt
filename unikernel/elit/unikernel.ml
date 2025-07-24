open Rresult
open Lwt.Infix

exception Invalid_certificate

let ( >>? ) = Lwt_result.bind

let ( $ ) f g = fun x -> match g x with Ok x -> f x | Error _ as err -> err
let ( <.> ) f g = fun x -> f (g x)
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let flip f y x = f x y

module K = struct
  open Cmdliner

  let dns_key =
    let doc = Arg.info ~doc:"nsupdate key" [ "dns-key" ] in
    let key = Arg.conv ~docv:"HOST:HASH:DATA" Dns.Dnskey.(name_key_of_string, Fmt.(using name_key_to_string string)) in
    Arg.(value & opt (some key) None doc)

  let dns_server =
    let doc = Arg.info ~doc:"dns server IP" [ "dns-server" ] in
    Arg.(value & opt (some Mirage_runtime_network.Arg.ip_address) None doc)

  let dns_port =
    let doc = Arg.info ~doc:"dns server port" [ "dns-port" ] in
    Arg.(value & opt int 53 doc)

  type dns =
    { ipaddr : Ipaddr.t
    ; port : int
    ; key : [ `raw ] Domain_name.t * Dns.Dnskey.t }

  let setup_dns_certify dns_key dns_server port =
    match dns_key, dns_server with
    | Some key, Some ipaddr -> Some { ipaddr; port; key }
    | _ -> None

  let setup_dns_certify =
    let open Term in
    const setup_dns_certify $ dns_key $ dns_server $ dns_port

  let domain =
    let doc = Arg.info ~doc:"SMTP domain-name." [ "domain" ] in
    let domain_name = Arg.conv Colombe.Domain.(of_string, pp) in
    Arg.(required & opt (some domain_name) None doc)

  let postmaster =
    let doc = Arg.info ~doc:"The postmaster of the SMTP service." [ "postmaster" ] in
    let mailbox = Arg.conv (Result.map_error (msgf "%a" Emile.pp_error) <.> Emile.of_string, Emile.pp_mailbox) in
    Arg.(required & opt (some mailbox) None doc)

  let submission =
    let doc = "The domain-name used for the submission service." in
    let parser = Domain_name.(host $ of_string) in
    Arg.(value & opt (some (conv (parser, Domain_name.pp))) None & info [ "submission" ] ~doc)

  let relay =
    let doc = "The domain-name used for the mail-exchange service." in
    let parser = Domain_name.(host $ of_string) in
    Arg.(value & opt (some (conv (parser, Domain_name.pp))) None & info [ "relay" ] ~doc)

  let destination =
    let doc = Arg.info ~doc:"Next SMTP server IP for submission" [ "destination" ] in
    Arg.(required & opt (some Mirage_runtime_network.Arg.ip_address) None doc)

  let mx_destination =
    let doc = Arg.info ~doc:"Next SMTP server IP for MX relay" [ "mx-destination" ] in
    Arg.(value & opt (some Mirage_runtime_network.Arg.ip_address) None doc)

  let seed =
    let doc = Arg.info [ "seed" ] ~doc:"The seed (in base64) of the private RSA key." in
    let parser str = Base64.decode str in
    let pp ppf str = Fmt.string ppf (Base64.encode_exn str) in
    let b64 = Arg.conv (parser, pp) in
    Arg.(value & opt (some b64) None doc)

  let forward_granted =
    let doc = "Allow to forward emails from IP addresses." in
    let ipaddr = Arg.conv (Ipaddr.Prefix.of_string, Ipaddr.Prefix.pp) in
    Arg.(value & opt_all ipaddr [] & info [ "forward-granted"] ~doc)

  let with_arc =
    let doc = "Emit an ARC-Authentication-Results instead of Authentication-Results." in
    Arg.(value & flag & info [ "with-arc" ] ~doc)

  type t =
    { domain : Colombe.Domain.t
    ; postmaster : Emile.mailbox
    ; dns : dns option
    ; submission : [ `host ] Domain_name.t option
    ; relay : [ `host ] Domain_name.t option
    ; seed : string option
    ; destination : Ipaddr.t
    ; mx_destination : Ipaddr.t option
    ; with_arc : bool
    ; forward_granted : Ipaddr.Prefix.t list }

  let setup domain postmaster dns submission relay seed destination mx_destination with_arc forward_granted =
    { domain; postmaster; dns; submission; relay; seed; destination; mx_destination; with_arc; forward_granted }

  let setup =
    let open Term in
    const setup $ domain $ postmaster $ setup_dns_certify $ submission $ relay $ seed $ destination $ mx_destination $ with_arc $ forward_granted
end

module Format = struct
  open Data_encoding

  exception Invalid_format

  let mailbox =
    let parser str = match Emile.of_string str with
      | Ok value -> value
      | Error _ -> raise Invalid_format in
    conv Emile.to_string parser string

  let blake2b =
    let parser str = match Digestif.BLAKE2B.of_hex_opt str with
      | Some value -> value
      | None -> raise Invalid_format in
    conv Digestif.BLAKE2B.to_hex parser string

  let local =
    let parser str =
      match Angstrom.parse_string
        ~consume:All Colombe.Path.Decoder.local_part str with
      | Ok value -> value
      | Error _ -> raise Invalid_format in
    let to_string = function
      | `String str -> str
      | `Dot_string sstr -> String.concat "." sstr in
    conv to_string parser string

  let user =
    let local = req "name" local in
    let password = req "password" blake2b in
    let mailboxes = req "mailboxes" (list mailbox) in
    obj3 local password mailboxes

  let database = list user
end

module Make
  (Block : Mirage_block.S)
  (Stack : Tcpip.Stack.V4V6)
  (Dns_client : Dns_client_mirage.S)
  (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow)
= struct
  module Certify = Dns_certify_mirage.Make (Stack)
  module Elit = Elit.Make (Stack) (Dns_client) (Happy_eyeballs)

  module Database = struct
    include OneFFS.Make (Block)

    let get t =
      read t >|= function
      | Error err -> Error (err :> [ error | `Msg of string ])
      | Ok None -> Ok (fun _fn -> Lwt.return_unit)
      | Ok (Some str) ->
        let ( let* ) = Result.bind in
        let* json =
          Data_encoding.Json.from_string str
          |> Result.map_error (fun msg -> `Msg msg) in
        let* db =
          try Ok (Data_encoding.Json.destruct Format.database json)
          with _ -> error_msgf "Invalid database" in
        Ok (fun fn -> Lwt_list.iter_p
          (fun (user, hash, local) -> fn user (Digestif.of_blake2b hash) local) db)
  end
  
  let pp_dns_key ppf (name, key) =
    Fmt.pf ppf "%a:%a:%s"
      Domain_name.pp name
      Dns.Dnskey.pp_algorithm key.Dns.Dnskey.algorithm
      key.Dns.Dnskey.key

  let retrieve_certs stack ~hostnames ~seed { K.key; ipaddr; port; } =
    let hostname, additional_hostnames = match hostnames with
      | [] -> failwith "You must ask for, at least, one domain"
      | x :: r -> x, Some (List.map Domain_name.raw r) in
    let key_seed = Domain_name.to_string hostname ^ ":" ^ seed in
    Certify.retrieve_certificate stack key
      ~key_seed ~hostname ?additional_hostnames ipaddr port >>= function
    | Error (`Msg err) -> failwith err
    | Ok certificates ->
      let now = Ptime.v (Mirage_ptime.now_d_ps ()) in
      let diffs =
        List.map (function (s :: _, _) -> s | _ -> assert false) [ certificates ]
        |> List.map X509.Certificate.validity
        |> List.map snd
        |> List.map (fun exp -> Ptime.diff exp now) in
      let next_expiration = Ptime.Span.to_d_ps (List.hd (List.sort Ptime.Span.compare diffs)) in
      let next_expiration = fst next_expiration in
      let seven_days_before_expiration = max (Duration.of_hour 1)
        (Duration.of_day (max 0 (next_expiration - 7))) in
      Lwt.return (`Single certificates, seven_days_before_expiration)

  let retrieve_certs stack ?submission ?relay ~seed dns =
    match submission, relay, dns with
    | None, Some relay, Some dns ->
      retrieve_certs stack ~hostnames:[ relay ] ~seed dns >>= fun (certificates, expiration) ->
      Lwt.return (None, Some certificates, expiration)
    | Some submission, None, Some dns ->
      retrieve_certs stack ~hostnames:[ submission ] ~seed dns >>= fun (certificates, expiration) ->
      Lwt.return (Some certificates, None, expiration)
    | Some submission, Some relay, Some dns when Domain_name.equal submission relay ->
      retrieve_certs stack ~hostnames:[ submission ] ~seed dns >>= fun (certificates, expiration) ->
      Lwt.return (Some certificates, Some certificates, expiration)
    | Some submission, Some relay, Some dns ->
      retrieve_certs stack ~hostnames:[ submission; relay ] ~seed dns >>= fun (certificates, expiration) ->
      Lwt.return (Some certificates, Some certificates, expiration)
    | Some _, None, None
    | None, Some _, None
    | Some _, Some _, None -> failwith "DNS informations are required to ask new certificates"
    | None, None, _ ->
      Lwt.return (None, None, Int64.max_int)

  let start block stack dns he
    ({ K.domain; postmaster; destination; seed; submission; relay; forward_granted; _ } as cfg) =
    let ( let* ) = Lwt.bind in
    let* oneffs = Database.connect block in
    let* iter = Database.get oneffs
      >|= Result.map_error (function
        | #Database.error as err -> msgf "%a" Database.pp_error err
        | `Msg msg -> `Msg msg)
      >|= R.failwith_error_msg in
    let ip = Stack.ip stack in
    let ipaddr = List.hd (Stack.IP.configured_ips ip) in
    let ipaddr = Ipaddr.Prefix.address ipaddr in
    let forward_granted = match forward_granted with
      | [] -> Stack.IP.configured_ips ip
      | forward_granted -> forward_granted in
    let info =
      { Ptt_common.domain
      ; ipaddr
      ; tls= None
      ; zone= Mrmime.Date.Zone.GMT
      ; size= 10_000_000L (* 10M *) } in
    let* server = Elit.v ~info ~postmaster ~forward_granted Digestif.BLAKE2B iter destination in
    let seed = match seed with
      | Some seed -> seed
      | None -> Mirage_crypto_rng.generate 16 in
    let rec loop (certs_submission, certs_relay, expiration) =
      let stop = Lwt_switch.create () in
      let tls_submission =
        (flip Option.map) certs_submission
        @@ fun certificates ->
        Rresult.R.failwith_error_msg (Tls.Config.server ~certificates ()) in
      let tls_relay =
        (flip Option.map) certs_relay
        @@ fun certificates ->
        Rresult.R.failwith_error_msg (Tls.Config.server ~certificates ()) in
      let wait_and_stop () =
        Mirage_sleep.ns expiration >>= fun () ->
        retrieve_certs stack ?submission ?relay ~seed cfg.dns >>= fun result ->
        Lwt_switch.turn_off stop >>= fun () ->
        Lwt.return result in
      let server () =
        Logs.debug (fun m -> m "Launch the elit server");
        Elit.job server ?submission:tls_submission ?relay:tls_relay
          (Stack.tcp stack) dns he in
      Lwt.both (server ()) (wait_and_stop ()) >>= fun ((), result) ->
      loop result in
    retrieve_certs stack ?submission ?relay ~seed cfg.dns >>= loop
end
