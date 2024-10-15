open Rresult
open Lwt.Infix

let ( $ ) f g = fun x -> match f x with Ok x -> g x | Error _ as err -> err
let ( <.> ) f g = fun x -> f (g x)
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt

module K = struct
  open Cmdliner

  let domain =
    let doc = Arg.info ~doc:"SMTP domain-name." [ "domain" ] in
    let domain_name = Arg.conv Colombe.Domain.(of_string, pp) in
    Arg.(required & opt (some domain_name) None doc)

  let postmaster =
    let doc = Arg.info ~doc:"The postmaster of the SMTP service." [ "postmaster" ] in
    let mailbox = Arg.conv (Result.map_error (msgf "%a" Emile.pp_error) <.> Emile.of_string, Emile.pp_mailbox) in
    Arg.(required & opt (some mailbox) None doc)

  let destination =
    let doc = Arg.info ~doc:"Next SMTP server IP" [ "destination" ] in
    Mirage_runtime.register_arg Arg.(required & opt (some Mirage_runtime_network.Arg.ip_address) None doc)

  type t =
    { domain : Colombe.Domain.t
    ; postmaster : Emile.mailbox }

  let v domain postmaster =
    { domain; postmaster }

  let setup = Term.(const v $ domain $ postmaster)
end

module Make
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
  (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow)
= struct

  module Nss = Ca_certs_nss.Make (Pclock)
  module Fake_dns = Ptt_fake_dns.Make (struct let ipaddr = K.destination () end)
  module Spam_filter = Spartacus.Make (Time) (Mclock) (Pclock) (Stack) (Fake_dns) (Happy_eyeballs)

  let start _time _mclock _pclock stack he { K.domain; postmaster }=
    let authenticator = R.failwith_error_msg (Nss.authenticator ()) in
    let tls = R.failwith_error_msg (Tls.Config.client ~authenticator ()) in
    let ip = Stack.ip stack in
    let ipaddr = List.hd (Stack.IP.configured_ips ip) in
    let ipaddr = Ipaddr.Prefix.address ipaddr in
    let info =
      { Ptt_common.domain
      ; ipaddr
      ; tls= None
      ; zone= Mrmime.Date.Zone.GMT
      ; size= 10_000_000L (* 10M *) } in
    let locals = Ptt_map.empty ~postmaster in
    Fake_dns.connect () >>= fun dns ->
    Spam_filter.job ~locals ~port:25 ~tls ~info (Stack.tcp stack) dns he
end
