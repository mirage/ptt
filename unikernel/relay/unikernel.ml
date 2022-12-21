open Rresult
open Lwt.Infix

let local_of_string str =
  match Angstrom.parse_string ~consume:All Emile.Parser.local_part str with
  | Ok v -> Ok v | Error _ -> Error (R.msgf "Invalid local-part: %S" str)

module Make
  (Random : Mirage_random.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
  (DNS : Dns_client_mirage.S with type Transport.stack = Stack.t
                              and type 'a Transport.io = 'a Lwt.t)
  (_ : sig end)
= struct
  module Store = Git_kv.Make (Pclock)

  module Resolver = struct
    type t = DNS.t
    type +'a io = 'a Lwt.t

    (* XXX(dinosaure): it seems that Gmail does not like IPv6... A solution
       will be to aggregate IPv4 and IPv6 from the given domain and return
       both to test both... *)
    let gethostbyname dns domain_name =
      DNS.gethostbyname dns domain_name >|= function
      | Ok ipv4 -> Ok (Ipaddr.V4 ipv4)
      | Error _ as err -> err

    let getmxbyname dns domain_name =
      DNS.getaddrinfo dns Dns.Rr_map.Mx domain_name >>= function
      | Ok (_ttl, mxs) -> Lwt.return_ok mxs
      | Error _ as err -> Lwt.return err

    let extension _dns ldh value =
      Lwt.return_error (R.msgf "[%s:%s] is not supported" ldh value)
  end

  module Mti_gf =
    Mti_gf.Make (Random) (Time) (Mclock) (Pclock) (Resolver) (Stack)
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
      let f acc = function
        | (_, `Dictionary) -> Lwt.return acc
        | (name, `Value) ->
          Store.get t name >>= function
          | Error err ->
            Logs.warn (fun m -> m "Got an error when we tried to get data \
                                   from %a: %a"
              Mirage_kv.Key.pp name Store.pp_error err);
            Lwt.return acc
          | Ok str ->
            match Ptt_value.of_string_json str, local_of_string (Mirage_kv.Key.to_string name) with
            | Ok { Ptt_value.targets; _ }, Ok local ->
              let acc = List.fold_left (fun acc x ->
                Ptt.Relay_map.add ~local x acc) acc targets in
              Lwt.return acc
            | _, Error (`Msg _) ->
              Logs.warn (fun m -> m "Invalid local-part: %a" Mirage_kv.Key.pp name);
              Lwt.return acc
            | Error (`Msg err), _ ->
              Logs.warn (fun m -> m "Invalid value for %a: %s" Mirage_kv.Key.pp name err);
              Lwt.return acc in
      Lwt_list.fold_left_s f relay_map values

  let start _random _time _mclock _pclock stack dns ctx =
    let domain = R.failwith_error_msg
      (Domain_name.of_string (Key_gen.domain ())) in
    let domain = Domain_name.host_exn domain in
    let postmaster =
      let postmaster = Key_gen.postmaster () in
      R.failwith_error_msg
        (R.reword_error (fun _ -> R.msgf "Invalid postmaster email: %S"
            postmaster)
          (Emile.of_string postmaster)) in
    let authenticator = R.failwith_error_msg (Nss.authenticator ()) in
    let tls = Tls.Config.client ~authenticator () in
    relay_map (Ptt.Relay_map.empty ~postmaster ~domain) ctx (Key_gen.remote ())
    >>= fun relay_map ->
    Mti_gf.fiber ~port:25 ~tls (Stack.tcp stack) dns relay_map
      { Ptt.Logic.domain
      ; ipaddr= Ipaddr.(V4 (V4.Prefix.address (Key_gen.ipv4 ())))
      ; tls= None
      ; zone= Mrmime.Date.Zone.GMT
      ; size= 10_000_000L (* 10M *) }
end
