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
  (_ : sig end)
= struct
  module Nss = Ca_certs_nss.Make (Pclock)
  module Certify = Dns_certify_mirage.Make (Random) (Pclock) (Time) (Stack)
  module Store = Git_kv.Make (Pclock)

  (* XXX(dinosaure): this is a fake resolver which enforce the [submission] to
   * transmit **any** emails to only one and unique SMTP server. *)

  module Resolver = struct
    type t = Ipaddr.t
    type +'a io = 'a Lwt.t

    let gethostbyname ipaddr _domain_name = Lwt.return_ok ipaddr
    let getmxbyname _ipaddr mail_exchange = Lwt.return_ok (Dns.Rr_map.Mx_set.singleton { Dns.Mx.preference= 0; mail_exchange; })
    let extension ipaddr _ldh _value = Lwt.return_ok ipaddr
  end

  module Lipap = Lipap.Make (Random) (Time) (Mclock) (Pclock) (Resolver) (Stack)

  let authentication ctx remote =
    Git_kv.connect ctx remote >>= fun t ->
    Store.list t Mirage_kv.Key.empty >>= function
    | Error err ->
      Logs.warn (fun m -> m "Got an error when we tried to list values from \
                             Git: %a."
        Store.pp_error err);
      let authentication =
        let open Ptt_tuyau.Lwt_backend.Lwt_scheduler in
        Ptt.Authentication.v
          (fun _local _v -> inj (Lwt.return false)) in
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
            match Ptt_value.of_string_json str, local_of_string (Mirage_kv.Key.to_string name) with
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
      let authentication =
        let open Ptt_tuyau.Lwt_backend.Lwt_scheduler in
        Ptt.Authentication.v
          (fun local v -> inj (authentication local v)) in
      Lwt.return authentication

  let retrieve_certs stack =
    let domain = Key_gen.submission_domain () in
    let key_seed = domain ^ ":" ^ (Key_gen.key_seed ()) in
    Certify.retrieve_certificate stack ~dns_key:(Key_gen.dns_key ())
      ~hostname:Domain_name.(host_exn (of_string_exn domain))
      ~key_seed (Key_gen.dns_server ()) 53 >>= function
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

  let start _random _time _mclock _pclock stack ctx =
    let domain = R.failwith_error_msg (Domain_name.of_string (Key_gen.domain ())) in
    let domain = Domain_name.host_exn domain in
    let postmaster =
      let postmaster = Key_gen.postmaster () in
      R.failwith_error_msg (R.reword_error (fun _ -> R.msgf "Invalid postmaster email: %S" postmaster)
        (Emile.of_string postmaster)) in
    let authenticator = R.failwith_error_msg (Nss.authenticator ()) in
    let tls = Tls.Config.client ~authenticator () in
    authentication ctx (Key_gen.remote ()) >>= fun authentication ->
    let rec loop (certificates, expiration) =
      let stop = Lwt_switch.create () in
      let wait_and_stop () =
        Time.sleep_ns expiration >>= fun () ->
        retrieve_certs stack >>= fun result ->
        Lwt_switch.turn_off stop >>= fun () ->
        Lwt.return result in
      let server () =
        Lipap.fiber ~port:465 ~tls (Stack.tcp stack) (Key_gen.destination ()) None Digestif.BLAKE2B
          (Ptt.Relay_map.empty ~postmaster ~domain)
          { Ptt.Logic.domain
          ; ipaddr= Ipaddr.(V4 (V4.Prefix.address (Key_gen.ipv4 ()))) (* XXX(dinosaure): or public IP address? *)
          ; tls= Some (Tls.Config.server ~certificates ())
          ; zone= Mrmime.Date.Zone.GMT
          ; size= 10_000_000L (* 10M *) }
          authentication [ Ptt.Mechanism.PLAIN ] in
      Lwt.both (server ()) (wait_and_stop ()) >>= fun ((), result) ->
      loop result in
    retrieve_certs stack >>= loop
end
