open Rresult
open Lwt.Infix

exception Invalid_certificate

let ( >>? ) = Lwt_result.bind
let ( <.> ) f g = fun x -> f (g x)

module Make
  (Random : Mirage_random.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
  (DNS : Dns_client_mirage.S with type Transport.stack = Stack.t
                              and type 'a Transport.io = 'a Lwt.t)
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

  module Verifier = Hm.Make (Random) (Time) (Mclock) (Pclock) (Resolver) (Stack) (DNS)
  module Nss = Ca_certs_nss.Make (Pclock)
  module Certify = Dns_certify_mirage.Make (Random) (Pclock) (Time) (Stack)

  let retrieve_certs stack =
    let domain = Key_gen.domain () in
    Certify.retrieve_certificate stack ~dns_key:(Key_gen.dns_key ())
      ~key_seed:(Key_gen.key_seed ()) ~hostname:Domain_name.(host_exn (of_string_exn domain))
      (Key_gen.dns_server ()) 53 >>= function
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

  let start _random _time _mclock _pclock stack dns =
    let postmaster =
      let postmaster = Key_gen.postmaster () in
      R.failwith_error_msg (R.reword_error (fun _ -> R.msgf "Invalid postmaster email: %S" postmaster)
        (Emile.of_string postmaster)) in
    let domain = R.failwith_error_msg (Domain_name.of_string (Key_gen.domain ())) in
    let domain = Domain_name.host_exn domain in
    let authenticator = R.failwith_error_msg (Nss.authenticator ()) in
    let tls = Tls.Config.client ~authenticator () in
    let rec loop (certificates, expiration) =
      let stop = Lwt_switch.create () in
      let wait_and_stop () =
        Time.sleep_ns expiration >>= fun () ->
        retrieve_certs stack >>= fun result ->
        Lwt_switch.turn_off stop >>= fun () ->
        Lwt.return result in
      let server () =
        Verifier.fiber ~port:25 ~tls (Stack.tcp stack) (Key_gen.destination ())
          { Ptt.Logic.domain
          ; ipaddr= Ipaddr.(V4 (V4.Prefix.address (Key_gen.ipv4 ())))
          ; tls= Some (Tls.Config.server ~certificates ())
          ; zone= Mrmime.Date.Zone.GMT
          ; size= 10_000_000L (* 10M *) }
          (Ptt.Relay_map.empty ~postmaster ~domain) dns in
      Lwt.both (server ()) (wait_and_stop ()) >>= fun ((), result) ->
      loop result in
    retrieve_certs stack >>= loop
end
