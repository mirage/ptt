open Rresult
open Lwt.Infix

exception Invalid_certificate

let ( >>? ) = Lwt_result.bind

let ( $ ) f g = fun x -> match f x with Ok x -> g x | Error _ as err -> err
let ( <.> ) f g = fun x -> f (g x)
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

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
    let key = Arg.conv ~docv:"HOST:HASH:DATA" Dns.Dnskey.(name_key_of_string, Fmt.(using name_key_to_string string)) in
    Arg.(required & opt (some key) None doc)

  let dns_server =
    let doc = Arg.info ~doc:"dns server IP" [ "dns-server" ] in
    Arg.(required & opt (some Mirage_runtime_network.Arg.ip_address) None doc)

  let dns_port =
    let doc = Arg.info ~doc:"dns server port" [ "dns-port" ] in
    Arg.(value & opt int 53 doc)

  let destination =
    let doc = Arg.info ~doc:"Next SMTP server IP" [ "destination" ] in
    Mirage_runtime.register_arg Arg.(required & opt (some Mirage_runtime_network.Arg.ip_address) None doc)

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

  let priv_of_seed ?(bits = 4096) (alg : Dkim.algorithm) seed : Dkim.key =
    match X509.Private_key.generate ~seed ~bits (alg :> X509.Key_type.t) with
    | #Dkim.key as key -> key
    | _ -> assert false
  
  let setup_key bits alg seed key =
    match (seed, key) with
    | None, Some key -> `Ok key
    | Some seed, None -> `Ok (priv_of_seed ?bits alg seed)
    | _, Some key -> `Ok key
    | None, None ->
        `Error (true, "A private key or a seed is required to sign an email.")

  let string_to_int_array str =
    let res = Array.make (String.length str / 2) 0 in
    for i = 0 to (String.length str / 2) - 1 do
      res.(i) <- (Char.code str.[i * 2] lsl 8) lor Char.code str.[(i * 2) + 1]
    done ;
    res

  let int_array_to_string arr =
    let buf = Bytes.create (Array.length arr * 2) in
    for i = 0 to Array.length arr - 1 do
      Bytes.set buf (2 * i) (Char.unsafe_chr (arr.(i) lsr 8)) ;
      Bytes.set buf ((2 * i) + 1) (Char.unsafe_chr arr.(i))
    done ;
    Bytes.unsafe_to_string buf

  let seed =
    let parser str = Base64.decode ~pad:true str in
    let pp ppf str = Fmt.string ppf (Base64.encode_string ~pad:true str) in
    Arg.conv (parser, pp)

  let seed =
    let doc =
      "Seed to generate a private key. Instead to pass a private-key, the user \
       can give a seed used then by a Fortuna random number generator to \
       generate a RSA private-key. From the seed, the user is able to reproduce \
       the same RSA private-key (and the public-key). The seed is a \
       $(b,base64-encoded) string." in
    Arg.(value & opt (some seed) None & info [ "seed" ] ~doc ~docv:"SEED")

  type key =
    [ `RSA of Mirage_crypto_pk.Rsa.priv
    | `ED25519 of Mirage_crypto_ec.Ed25519.priv ]
  
  let private_key : key Arg.conv =
    let parser str =
      let ( let* ) = Result.bind in
      let* key = Base64.decode ~pad:true str in
      match X509.Private_key.decode_der key with
      | Ok #key as key -> key
      | Ok _ -> error_msgf "Invalid algorithm used for DKIM signature"
      | Error _ as err -> err in
    let pp ppf (pk : key) =
      Fmt.string ppf (X509.Private_key.encode_der (pk :> X509.Private_key.t))
    in
    Arg.conv (parser, pp)

  let pot x = x land (x - 1) == 0 && x != 0

  let bits =
    let parser str =
      try
        let v = int_of_string str in
        if pot v then Ok v else error_msgf "The given value is not a power of two"
      with _ -> error_msgf "Invalid number" in
    Arg.conv (parser, Fmt.int)

  let algorithm =
    let parser str =
      match String.trim (String.lowercase_ascii str) with
      | "rsa" -> Ok `RSA
      | "ed25519" -> Ok `ED25519
      | _ -> error_msgf "Invalid algorithm: %S" str in
    let pp ppf = function
      | `RSA -> Fmt.string ppf "rsa"
      | `ED25519 -> Fmt.string ppf "ed25519" in
    Arg.conv (parser, pp)
  
  let private_key =
    let doc = "The X.509 PEM encoded private key used to sign the email." in
    Arg.(value & opt (some private_key) None & info [ "p" ] ~doc)
  
  let bits =
    let doc = "Size of key in bits." in
    Arg.(value & opt (some bits) None & info [ "b"; "bits" ] ~doc ~docv:"NUMBER")
  
  let algorithm =
    let doc = "The algorithm use to encrypt/decrypt signatures." in
    let open Arg in
    value & opt algorithm `RSA & info [ "a"; "algorithm" ] ~doc ~docv:"ALGORITHM"
  
  let setup_key =
    let open Term in
    const setup_key $ bits $ algorithm $ seed $ private_key |> ret

  type t =
    { domain : [ `host ] Domain_name.t
    ; postmaster : Emile.mailbox
    ; dns_key : [ `raw ] Domain_name.t * Dns.Dnskey.t
    ; dns_server : Ipaddr.t
    ; dns_port : int
    ; fields : Mrmime.Field_name.t list option
    ; selector : [ `raw ] Domain_name.t
    ; timestamp : int64 option
    ; expiration : int64 option
    ; private_key : Dkim.key }

  let v domain postmaster  dns_key dns_server dns_port fields selector timestamp expiration private_key =
    { domain
    ; postmaster
    ; dns_key
    ; dns_server
    ; dns_port
    ; fields
    ; selector
    ; timestamp
    ; expiration
    ; private_key }

  let setup =
    let open Term in
    const v
    $ domain
    $ postmaster
    $ dns_key
    $ dns_server
    $ dns_port
    $ fields
    $ selector
    $ timestamp
    $ expiration
    $ setup_key
end

module Make
  (Stack : Tcpip.Stack.V4V6)
  (Dns_client : Dns_client_mirage.S)
  (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow)
= struct
  (* XXX(dinosaure): this is a fake resolver which enforce the [signer] to
   * transmit **any** emails to only one and unique SMTP server. *)

  module DKIM = Dkim_mirage.Make (Dns_client)

  let private_rsa_key_from_seed seed =
    let g = Mirage_crypto_rng.(create ~seed (module Fortuna)) in
    Mirage_crypto_pk.Rsa.generate ~g ~bits:2048 ()

  let ns_check dkim dk dns =
    let domain_name = Dkim.Verify.domain_key dkim in
    Lwt.return domain_name >>? fun domain_name ->
    Dns_client.get_resource_record dns Dns.Rr_map.Txt domain_name >>? fun (_ttl, value') ->
    let value' = Dns.Rr_map.Txt_set.to_list value' in
    let dk' = Dkim.domain_key_of_string (String.concat "" value') in
    Lwt.return dk' >>? fun dk' ->
    if Dkim.equal_domain_key dk dk'
    then Lwt.return_ok `Already_registered
    else Lwt.return_ok `Must_be_updated

  module DNS = Dns_mirage.Make (Stack)

  let ns_update (key_name, dns_key) dkim value stack dns { K.dns_server; dns_port; _ } =
    ns_check dkim value dns >>= function
    | Ok `Already_registered -> Lwt.return_ok ()
    | Ok `Must_be_updated | Error _ ->
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
        Dns.Rr_map.(B (Txt, (3600l, Txt_set.singleton (Dkim.domain_key_to_string value)))) in
      let packet =
        let header = (Randomconv.int16 Mirage_crypto_rng.generate, Dns.Packet.Flags.empty) in
        let zone = Dns.Packet.Question.create key_zone Dns.Rr_map.Soa in
        Dns.Packet.create header zone (`Update Domain_name.Map.(empty, singleton dkim_domain [ v ])) in
      Lwt.bind
        begin
          Dns_tsig.encode_and_sign ~proto:`Tcp packet (Ptime.v (Mirage_ptime.now_d_ps ()))
          dns_key key_name |> R.reword_error (R.msgf "%a" Dns_tsig.pp_s) |> Lwt.return >>? fun (data, mac) ->
          DNS.send_tcp flow (Cstruct.of_string data) 
          >|= R.reword_error (fun _ -> R.msgf "Impossible to send a DNS packet to %a:%d"
            Ipaddr.pp dns_server dns_port) >>? fun () -> DNS.read_tcp (DNS.of_flow flow)
          >|= R.reword_error (fun _ -> R.msgf "Impossible to read a DNS packet from %a:%d"
            Ipaddr.pp dns_server dns_port) >>? fun data ->
          let data = Cstruct.to_string data in
          Dns_tsig.decode_and_verify (Ptime.v (Mirage_ptime.now_d_ps ())) dns_key key_name ~mac data
          |> R.reword_error (R.msgf "%a" Dns_tsig.pp_e)
          |> Lwt.return >>? fun (packet', _tsig, _mac) ->
          match Dns.Packet.reply_matches_request ~request:packet packet' with
          | Ok _ -> Lwt.return_ok ()
          | Error _ -> assert false
        end @@ fun res -> Stack.TCP.close flow >>= fun () -> Lwt.return res

  module Fake_dns = Ptt_fake_dns.Make (struct let ipaddr = K.destination () end)
  module Nec = Nec.Make (Stack) (Fake_dns) (Happy_eyeballs)

  let start stack dns he
    ({ K.domain; postmaster; dns_key; fields; selector; private_key; timestamp; expiration; _ } as cfg) =
    let dkim = Dkim.v
      ~version:1 ?fields ~selector
      ~algorithm:`RSA
      ~query:(`DNS (`TXT))
      ?timestamp
      ?expiration
      (Domain_name.raw domain) in
    let value = Dkim.domain_key_of_dkim ~key:private_key dkim in
    let authenticator = R.failwith_error_msg (Ca_certs_nss.authenticator ()) in
    let tls = R.failwith_error_msg (Tls.Config.client ~authenticator ()) in
    ns_update dns_key dkim value stack dns cfg >|= R.failwith_error_msg >>= fun () ->
    let ip = Stack.ip stack in
    let ipaddr = List.hd (Stack.IP.configured_ips ip) in
    let ipaddr = Ipaddr.Prefix.address ipaddr in
    let locals = Ptt_map.empty ~postmaster in
    let info =
      { Ptt_common.domain= Colombe.Domain.Domain (Domain_name.to_strings domain)
      ; ipaddr
      ; tls= None
      ; zone= Mrmime.Date.Zone.GMT
      ; size= 10_000_000L } in
    Fake_dns.connect () >>= fun dns ->
    Nec.job ~locals ~port:25 ~tls ~info (Stack.tcp stack) dns he (private_key, dkim)
end
