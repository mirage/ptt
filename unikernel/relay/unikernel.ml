open Rresult
open Lwt.Infix

let ( >>? ) = Lwt_result.bind
let ( <.> ) f g = fun x -> f (g x)

module Maker = Irmin_mirage_git.KV (Irmin_git.Mem)
module Store = Maker.Make (Ptt_irmin)
module Sync = Irmin.Sync.Make (Store)

let failwith_error_pull = function
  | Error (`Conflict err) -> Fmt.failwith "Conflict: %s" err
  | Error (`Msg err) -> failwith err
  | Ok v -> v

let local_of_string str =
  match Angstrom.parse_string ~consume:All Emile.Parser.local_part str with
  | Ok v -> v | Error _ -> Fmt.failwith "Invalid local-part: %S" str

module Make
  (Random : Mirage_random.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Mirage_stack.V4V6)
  (_ : sig end)
= struct
  module Resolver = struct
    include Dns_client_mirage.Make (Random) (Time) (Mclock) (Stack)

    type +'a io = 'a Lwt.t

    let gethostbyname dns domain_name = gethostbyname dns domain_name
    let getmxbyname dns domain_name = getaddrinfo dns Dns.Rr_map.Mx domain_name >>= function
      | Ok (_ttl, mxs) -> Lwt.return_ok mxs
      | Error _ as err -> Lwt.return err
    let extension _dns ldh value = Lwt.return_error (R.msgf "[%s:%s] is not supported" ldh value)
  end

  module Mti_gf = Mti_gf.Make (Random) (Time) (Mclock) (Pclock) (Resolver) (Stack)
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

  let relay_map relay_map ctx remote =
    let config = Irmin_mem.config () in
    Store.Repo.v config >>= Store.master >>= fun store ->
    let upstream = Store.remote ~ctx remote in
    Sync.pull store upstream `Set
    >|= failwith_error_pull
    >>= fun _ ->
    Store.(list store (Schema.Path.v [])) >>= fun values ->
    let f acc (name, k) = match Store.Tree.destruct k with
      | `Node _ -> Lwt.return acc
      | `Contents _ ->
        Store.(get store (Schema.Path.v [ name ])) >>= fun { Ptt_irmin.targets; _ } ->
        let local = local_of_string name in
        let acc = List.fold_left (fun acc x -> Ptt.Relay_map.add ~local x acc) acc targets in
        Lwt.return acc in
    Lwt_list.fold_left_s f relay_map values

  let start _random _time _mclock _pclock stack ctx =
    let nameservers = match Key_gen.resolver () with
      | None -> None
      | Some nameserver ->
        let nameserver = Uri.of_string nameserver in
        let protocol = match Uri.scheme nameserver with
          | Some "tcp" -> `Tcp
          | Some "udp" | _ -> `Udp in
        let ipaddr = Option.bind (Uri.host nameserver) (R.to_option <.> Ipaddr.of_string) in
        let port = Option.value (Uri.port nameserver) ~default:53 in
        match ipaddr with
        | Some ipaddr -> Some (protocol, [ ipaddr, port ])
        | None -> None in
    let dns = Resolver.create ?nameservers stack in
    let domain = R.failwith_error_msg (Domain_name.of_string (Key_gen.domain ())) in
    let domain = Domain_name.host_exn domain in
    let postmaster =
      let postmaster = Key_gen.postmaster () in
      R.failwith_error_msg (R.reword_error (fun _ -> R.msgf "Invalid postmaster email: %S" postmaster)
        (Emile.of_string postmaster)) in
    let authenticator = R.failwith_error_msg (Nss.authenticator ()) in
    let tls = Tls.Config.client ~authenticator () in
    certificate () |> Lwt.return >|= R.failwith_error_msg >>= fun certificates ->
    relay_map (Ptt.Relay_map.empty ~postmaster ~domain) ctx (Key_gen.remote ()) >>= fun relay_map ->
    Mti_gf.fiber ~port:25 ~tls stack dns relay_map
      { Ptt.Logic.domain
      ; ipv4= (Ipaddr.V4.Prefix.address (Key_gen.ipv4 ()))
      ; tls= Tls.Config.server ~certificates ()
      ; zone= Mrmime.Date.Zone.GMT (* XXX(dinosaure): any MirageOS use GMT. *)
      ; size= 10_000_000L (* 10M *) }
end
