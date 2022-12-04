open Rresult
open Lwt.Infix

module Make
  (Random : Mirage_random.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
= struct
  (* XXX(dinosaure): this is a fake resolver which enforce the [filter] to
   * transmit **any** emails to only one and unique SMTP server. *)

  module Resolver = struct
    type t = Ipaddr.t
    type +'a io = 'a Lwt.t

    let gethostbyname ipaddr _domain_name = Lwt.return_ok ipaddr
    let getmxbyname _ipaddr mail_exchange = Lwt.return_ok (Dns.Rr_map.Mx_set.singleton { Dns.Mx.preference= 0; mail_exchange; })
    let extension ipaddr _ldh _value = Lwt.return_ok ipaddr
  end

  module SpamFilter = Spartacus.Make (Random) (Time) (Mclock) (Pclock) (Resolver) (Stack)
  module Nss = Ca_certs_nss.Make (Pclock)

  let start _random _time _mclock _pclock stack =
    let postmaster =
      let postmaster = Key_gen.postmaster () in
      R.failwith_error_msg (R.reword_error (fun _ -> R.msgf "Invalid postmaster email: %S" postmaster)
        (Emile.of_string postmaster)) in
    let domain = R.failwith_error_msg (Domain_name.of_string (Key_gen.domain ())) in
    let authenticator = R.failwith_error_msg (Nss.authenticator ()) in
    let tls = Tls.Config.client ~authenticator () in
    let domain = Domain_name.host_exn domain in
    SpamFilter.fiber ~port:25 ~tls (Stack.tcp stack) (Key_gen.destination ())
      (Ptt.Relay_map.empty ~postmaster ~domain)
      { Ptt.Logic.domain
      ; ipaddr= Ipaddr.(V4 (V4.Prefix.address (Key_gen.ipv4 ())))
      ; tls= None
      ; zone= Mrmime.Date.Zone.GMT (* XXX(dinosaure): any MirageOS use GMT. *)
      ; size= 10_000_000L (* 10M *) }
end
