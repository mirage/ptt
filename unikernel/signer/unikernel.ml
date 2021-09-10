open Rresult
open Lwt.Infix

exception Invalid_certificate

let ( >>? ) = Lwt_result.bind

module Make
  (Random : Mirage_random.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Mirage_stack.V4V6)
= struct
  (* XXX(dinosaure): this is a fake resolver which enforce the [signer] to
   * transmit **any** emails to only one and unique SMTP server. *)

  module Resolver = struct
    type t = Ipaddr.V4.t
    type +'a io = 'a Lwt.t

    let gethostbyname ipaddr _domain_name = Lwt.return_ok ipaddr
    let getmxbyname _ipaddr mail_exchange = Lwt.return_ok (Dns.Rr_map.Mx_set.singleton { Dns.Mx.preference= 0; mail_exchange; })
    let extension ipaddr _ldh _value = Lwt.return_ok ipaddr
  end

  module Nec = Nec.Make (Random) (Time) (Mclock) (Pclock) (Resolver) (Stack)
  module DKIM = Dkim_mirage.Make (Random) (Time) (Mclock) (Pclock) (Stack)
  module DNS = Dns_mirage.Make (Stack)
  module Nss = Ca_certs_nss.Make (Pclock)

  let private_rsa_key_from_seed seed =
    let g =
      let seed = Cstruct.of_string seed in
      Mirage_crypto_rng.(create ~seed (module Fortuna)) in
    Mirage_crypto_pk.Rsa.generate ~g ~bits:2048 ()

  let ns_check dkim server stack =
    DKIM.server ~nameserver:(`Tcp, (Key_gen.dns_server (), Key_gen.dns_port ())) stack dkim >>= function
    | Ok server' ->
      Logs.info (fun m -> m "The DNS server already has a DKIM public key: %a (expected: %a)."
        Dkim.pp_server server' Dkim.pp_server server) ;
      if Dkim.equal_server server server'
      then Lwt.return `Already_registered else Lwt.return `Must_be_updated
    | Error _ ->
      Logs.info (fun m -> m "The DNS server does not have the DKIM public key.") ;
      Lwt.return `Not_found

  let ns_update dkim server stack =
    ns_check dkim server stack >>= function
    | `Already_registered -> Lwt.return_ok ()
    | `Must_be_updated | `Not_found ->
      let dkim_domain = R.failwith_error_msg (Dkim.domain_name dkim) in
      let key_name, key_zone, dns_key =
        let key_name, dns_key = R.failwith_error_msg (Dns.Dnskey.name_key_of_string (Key_gen.dns_key ())) in
        match Domain_name.find_label key_name (function "_update" -> true | _ -> false) with
        | None -> Fmt.failwith "The given DNS key is not an update key"
        | Some idx ->
          let amount = succ idx in
          let zone = Domain_name.(host_exn (drop_label_exn ~amount key_name)) in
          key_name, zone, dns_key in
      Stack.TCP.create_connection (Stack.tcp stack) (Key_gen.dns_server (), Key_gen.dns_port ())
      >|= R.reword_error (R.msgf "%a" Stack.TCP.pp_error) >>? fun flow ->
      let v = Dns.Packet.Update.Add
        Dns.Rr_map.(B (Txt, (3600l, Txt_set.singleton (Dkim.server_to_string server)))) in
      let packet =
        let header = (Randomconv.int16 Random.generate, Dns.Packet.Flags.empty) in
        let zone = Dns.Packet.Question.create key_zone Dns.Rr_map.Soa in
        Dns.Packet.create header zone (`Update Domain_name.Map.(empty, singleton dkim_domain [ v ])) in
      Lwt.bind
        begin
          Dns_tsig.encode_and_sign ~proto:`Tcp packet (Ptime.v (Pclock.now_d_ps ()))
          dns_key key_name |> R.reword_error (R.msgf "%a" Dns_tsig.pp_s) |> Lwt.return >>? fun (data, mac) ->
          DNS.send_tcp flow data 
          >|= R.reword_error (fun _ -> R.msgf "Impossible to send a DNS packet to %a:%d"
            Ipaddr.pp (Key_gen.dns_server ())
            (Key_gen.dns_port ())) >>? fun () -> DNS.read_tcp (DNS.of_flow flow)
          >|= R.reword_error (fun _ -> R.msgf "Impossible to read a DNS packet from %a:%d"
            Ipaddr.pp (Key_gen.dns_server ())
            (Key_gen.dns_port ())) >>? fun data ->
          Dns_tsig.decode_and_verify (Ptime.v (Pclock.now_d_ps ())) dns_key key_name ~mac data
          |> R.reword_error (R.msgf "%a" Dns_tsig.pp_e)
          |> Lwt.return >>? fun (packet', _tsig, _mac) ->
          match Dns.Packet.reply_matches_request ~request:packet packet' with
          | Ok _ -> Lwt.return_ok ()
          | Error _ -> assert false
        end @@ fun res -> Stack.TCP.close flow >>= fun () -> Lwt.return res

  let certificate () =
    let open Rresult in
    Base64.decode (Key_gen.cert_der ()) |> R.reword_error (fun _ -> R.msgf "Invalid DER certificate")
    >>| Cstruct.of_string >>= X509.Certificate.decode_der >>= fun certificate ->
    Base64.decode (Key_gen.cert_key ()) |> R.reword_error (fun _ -> R.msgf "Invalid private key")
    >>| Cstruct.of_string >>= fun seed ->
    let g = Mirage_crypto_rng.(create ~seed (module Fortuna)) in
    let private_key = Mirage_crypto_pk.Rsa.generate ~g ~bits:2048 () in
    R.ok (`Single ([ certificate ], `RSA private_key))

  let time () = match Ptime.v (Pclock.now_d_ps ()) with
    | v -> Some v | exception _ -> None

  let parse_alg str = match String.lowercase_ascii str with
    | "sha1" -> Ok `SHA1
    | "sha256" -> Ok `SHA256
    | _ -> R.error_msgf "Invalid hash algorithm: %S" str

  let authenticator () = match Key_gen.key_fingerprint (), Key_gen.cert_fingerprint () with
    | None, None -> Nss.authenticator ()
    | Some str, _ ->
      let res = match String.split_on_char ':' str with
        | [ host; alg; fingerprint ] ->
          let open Rresult in
          Domain_name.of_string host >>= Domain_name.host >>= fun host ->
          parse_alg alg >>= fun alg ->
          Base64.decode ~pad:false fingerprint >>= fun fingerprint ->
          R.ok (host, alg, fingerprint)
        | _ -> R.error_msgf "Invalid key fingerprint." in
      let (host, hash, fingerprint) = R.failwith_error_msg res in
      R.ok @@ X509.Authenticator.server_key_fingerprint ~time ~hash ~fingerprints:[ host, Cstruct.of_string fingerprint ]
    | None, Some str ->
      let res = match String.split_on_char ':' str with
        | [ host; alg; fingerprint ] ->
          let open Rresult in
          Domain_name.of_string host >>= Domain_name.host >>= fun host ->
          parse_alg alg >>= fun alg ->
          Base64.decode ~pad:false fingerprint >>= fun fingerprint ->
          R.ok (host, alg, fingerprint)
        | _ -> R.error_msgf "Invalid key fingerprint." in
      let (host, hash, fingerprint) = R.failwith_error_msg res in
      R.ok @@ X509.Authenticator.server_cert_fingerprint ~time ~hash ~fingerprints:[ host, Cstruct.of_string fingerprint ]

  let start _random _time _mclock _pclock stack =
    let fields = match Key_gen.fields () with
      | None -> None
      | Some fields ->
        let fields = String.split_on_char ':' fields in
        let f acc x = match acc with Error _ as err -> err | Ok acc ->
          match Mrmime.Field_name.of_string x with
          | Ok x -> Ok (x :: acc)
          | Error _ -> R.error_msgf "Invalid field-name: %S" x in
        let fields = R.failwith_error_msg (List.fold_left f (Ok []) fields) in
        Some fields in
    let postmaster =
      let postmaster = Key_gen.postmaster () in
      R.failwith_error_msg (R.reword_error (fun _ -> R.msgf "Invalid postmaster email: %S" postmaster)
        (Emile.of_string postmaster)) in
    let selector = R.failwith_error_msg (Domain_name.of_string (Key_gen.selector ())) in
    let domain = R.failwith_error_msg (Domain_name.of_string (Key_gen.domain ())) in
    let dkim = Dkim.v
      ~version:1 ?fields ~selector
      ~algorithm:`RSA
      ~query:(`DNS (`TXT))
      ?timestamp:(Option.map Int64.of_int (Key_gen.timestamp ()))
      ?expiration:(Option.map Int64.of_int (Key_gen.expiration ()))
      domain in
    let private_key = private_rsa_key_from_seed (Base64.decode_exn (Key_gen.private_key ())) in
    let server = Dkim.server_of_dkim ~key:private_key dkim in
    let authenticator = R.failwith_error_msg (authenticator ()) in
    let tls = Tls.Config.client ~authenticator () in
    ns_update dkim server stack >|= R.failwith_error_msg >>= fun () ->
    certificate () |> Lwt.return >|= R.failwith_error_msg >>= fun certificates ->
    let domain = Domain_name.host_exn domain in
    Nec.fiber ~port:25 ~tls stack (Key_gen.destination ()) (private_key, dkim)
      (Ptt.Relay_map.empty ~postmaster ~domain)
      { Ptt.Logic.domain
      ; ipv4= (Ipaddr.V4.Prefix.address (Key_gen.ipv4 ()))
      ; tls= Tls.Config.server ~certificates ()
      ; zone= Mrmime.Date.Zone.GMT (* XXX(dinosaure): any MirageOS use GMT. *)
      ; size= 10_000_000L (* 10M *) }
end
