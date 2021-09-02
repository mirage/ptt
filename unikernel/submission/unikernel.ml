open Rresult
open Lwt.Infix

let ( >>? ) = Lwt_result.bind

module Store = Irmin_mirage_git.Mem.KV (Ptt_irmin)
module Sync = Irmin.Sync (Store)

let failwith_error_pull = function
  | Error (`Conflict err) -> Fmt.failwith "Conflict: %s" err
  | Error (`Msg err) -> failwith err
  | Ok v -> v

let local_to_string local =
  let pp ppf = function `Atom x -> Fmt.string ppf x
    | `String v -> Fmt.pf ppf "%S" v in
  Fmt.str "%a" Fmt.(list ~sep:(always ".") pp) local

module Make
  (Random : Mirage_random.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Disk : Mirage_kv.RO)
  (Stack : Mirage_stack.V4V6)
  (_ : sig end)
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

  module Lipap = Lipap.Make (Random) (Time) (Mclock) (Pclock) (Resolver) (Stack)

  let authentication ctx remote =
    let tree = Art.make () in
    let config = Irmin_mem.config () in
    Store.Repo.v config >>= Store.master >>= fun store ->
    let upstream = Store.remote ~ctx remote in
    Sync.pull store upstream `Set
    >|= failwith_error_pull
    >>= fun _ ->
    Store.list store [] >>= fun values ->
    let f () (name, k) = match Store.Tree.destruct k with
      | `Node _ -> Lwt.return_unit
      | `Contents _ ->
        Store.get store [ name ] >>= fun { Ptt_irmin.password; _ } ->
        let local = Art.key name in
        Art.insert tree local password ; Lwt.return_unit in
    Lwt_list.fold_left_s f () values >>= fun () ->
    let authentication local v' =
      match Art.find tree (Art.key (local_to_string local)) with
      | v -> Lwt.return (Digestif.equal Digestif.BLAKE2B (Digestif.of_blake2b v) v')
      | exception _ -> Lwt.return false in
    let authentication =
      let open Ptt_tuyau.Lwt_backend.Lwt_scheduler in
      Ptt.Authentication.v
        (fun local v -> inj (authentication local v)) in
    Lwt.return authentication

  let certificates disk =
    Disk.get disk (Mirage_kv.Key.v "server.pem")
    >|= R.reword_error (fun _ -> R.msgf "We need a TLS certificate (PEM format)")
    >>? fun pem ->
    Disk.get disk (Mirage_kv.Key.v "server.key")
    >|= R.reword_error (fun _ -> R.msgf "We need the private key for the TLS certificate (PEM format)")
    >>? fun key ->
    let pem = Cstruct.of_string pem in
    let key = Cstruct.of_string key in
    match X509.Certificate.decode_pem_multiple pem, X509.Private_key.decode_pem key with
    | Ok crts, Ok key -> Lwt.return_ok (`Single (crts, key))
    | _ -> Lwt.return_error (R.msgf "Invalid certificate or private key")

  let start _random _time _mclock _pclock disk stack ctx =
    let domain = R.failwith_error_msg (Domain_name.of_string (Key_gen.domain ())) in
    let domain = Domain_name.host_exn domain in
    let postmaster =
      let postmaster = Key_gen.postmaster () in
      R.failwith_error_msg (R.reword_error (fun _ -> R.msgf "Invalid postmaster email: %S" postmaster)
        (Emile.of_string postmaster)) in
    certificates disk >|= R.failwith_error_msg >>= fun certificates ->
    authentication ctx (Key_gen.remote ()) >>= fun authentication ->
    Lipap.fiber ~port:465 stack (Key_gen.destination ()) None Digestif.BLAKE2B
      (Ptt.Relay_map.empty ~postmaster ~domain)
      { Ptt.Logic.domain
      ; ipv4= (Ipaddr.V4.Prefix.address (Key_gen.ipv4 ()))
      ; tls= Tls.Config.server ~certificates ()
      ; zone= Mrmime.Date.Zone.GMT (* XXX(dinosaure): any MirageOS use GMT. *)
      ; size= 10_000_000L (* 10M *) }
      authentication [ Ptt.Mechanism.PLAIN ]
end
