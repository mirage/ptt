open Rresult
open Lwt.Infix

exception Invalid_certificate

let ( >>? ) = Lwt_result.bind

module Make
  (Random : Mirage_random.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
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

  module SpamFilter = Spartacus.Make (Random) (Time) (Mclock) (Pclock) (Resolver) (Stack)
  module Nss = Ca_certs_nss.Make (Pclock)

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
      let (_host, hash, fingerprint) = R.failwith_error_msg res in
      R.ok @@ X509.Authenticator.server_key_fingerprint ~time ~hash ~fingerprint:(Cstruct.of_string fingerprint)
    | None, Some str ->
      let res = match String.split_on_char ':' str with
        | [ host; alg; fingerprint ] ->
          let open Rresult in
          Domain_name.of_string host >>= Domain_name.host >>= fun host ->
          parse_alg alg >>= fun alg ->
          Base64.decode ~pad:false fingerprint >>= fun fingerprint ->
          R.ok (host, alg, fingerprint)
        | _ -> R.error_msgf "Invalid key fingerprint." in
      let (_host, hash, fingerprint) = R.failwith_error_msg res in
      R.ok @@ X509.Authenticator.server_cert_fingerprint ~time ~hash ~fingerprint:(Cstruct.of_string fingerprint)

  let start _random _time _mclock _pclock stack =
    let postmaster =
      let postmaster = Key_gen.postmaster () in
      R.failwith_error_msg (R.reword_error (fun _ -> R.msgf "Invalid postmaster email: %S" postmaster)
        (Emile.of_string postmaster)) in
    let domain = R.failwith_error_msg (Domain_name.of_string (Key_gen.domain ())) in
    let authenticator = R.failwith_error_msg (authenticator ()) in
    let tls = Tls.Config.client ~authenticator () in
    certificate () |> Lwt.return >|= R.failwith_error_msg >>= fun certificates ->
    let domain = Domain_name.host_exn domain in
    SpamFilter.fiber ~port:25 ~tls (Stack.tcp stack) (Key_gen.destination ())
      (Ptt.Relay_map.empty ~postmaster ~domain)
      { Ptt.Logic.domain
      ; ipv4= (Ipaddr.V4.Prefix.address (Key_gen.ipv4 ()))
      ; tls= Tls.Config.server ~certificates ()
      ; zone= Mrmime.Date.Zone.GMT (* XXX(dinosaure): any MirageOS use GMT. *)
      ; size= 10_000_000L (* 10M *) }
end
