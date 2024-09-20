open Rresult
open Lwt.Infix

let ( $ ) f g = fun x -> match f x with Ok x -> g x | Error _ as err -> err
let ( <.> ) f g = fun x -> f (g x)
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt

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

  let destination =
    let doc = Arg.info ~doc:"Next SMTP server IP" [ "destination" ] in
    Arg.(required & opt (some Mirage_runtime_network.Arg.ip_address) None doc)

  type t =
    { domain : [ `host ] Domain_name.t
    ; postmaster : Emile.mailbox
    ; destination : Ipaddr.t }

  let v domain postmaster destination =
    { domain; postmaster; destination }

  let setup = Term.(const v $ domain $ postmaster $ destination)
end

module Make
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

  module SpamFilter = Spartacus.Make (Time) (Mclock) (Pclock) (Resolver) (Stack)
  module Nss = Ca_certs_nss.Make (Pclock)

  let start _time _mclock _pclock stack { K.domain; postmaster; destination }=
    let authenticator = R.failwith_error_msg (Nss.authenticator ()) in
    let tls = R.failwith_error_msg (Tls.Config.client ~authenticator ()) in
    let ip = Stack.ip stack in
    let ipaddr = List.hd (Stack.IP.configured_ips ip) in
    let ipaddr = Ipaddr.Prefix.address ipaddr in
    SpamFilter.fiber ~port:25 ~tls (Stack.tcp stack) destination
      { Ptt.Logic.domain
      ; ipaddr
      ; tls= None
      ; zone= Mrmime.Date.Zone.GMT (* XXX(dinosaure): any MirageOS use GMT. *)
      ; size= 10_000_000L (* 10M *) }
end
