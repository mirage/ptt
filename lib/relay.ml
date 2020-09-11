open Sigs
open Rresult

module Make
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
    (Flow : FLOW with type 'a io = 'a IO.t)
    (Resolver : RESOLVER with type 'a io = 'a IO.t)
    (Random : RANDOM with type 'a io = 'a IO.t)
= struct
  include Common.Make (Scheduler) (IO) (Flow) (Resolver) (Random)
  module Md = Messaged.Make(Scheduler)(IO)

  let src = Logs.Src.create "ptt-relay"
  module Log = (val Logs.src_log src : Logs.LOG)

  type server =
    { info : info
    ; messaged : Md.t
    ; mutable count : int64 }
  and info = SMTP.info =
    { domain : [ `host ] Domain_name.t
    ; ipv4 : Ipaddr.V4.t
    ; tls : Tls.Config.server
    ; zone : Mrmime.Date.Zone.t
    ; size : int64 }

  let info { info; _ } = info

  let create ~info =
    { info
    ; messaged= Md.create ()
    ; count= 0L }

  let messaged { messaged; _ } = messaged

  let succ server =
    let v = server.count in
    server.count <- Int64.succ server.count ; v

  type error =
    [ `Error of SMTP.error
    | `Connection_close
    | `Too_big_data ]

  let pp_error ppf = function
    | `Error err -> SMTP.pp_error ppf err
    | `Connection_close -> Fmt.pf ppf "Connection close"
    | `Too_big_data -> Fmt.pf ppf "Too big data"

  let properly_close_tls flow ctx =
    let encoder = Sendmail_with_starttls.Context_with_tls.encoder ctx in
    let tls_error err = `Tls err in
    let m = SMTP.Value_with_tls.close encoder |> SMTP.Monad.reword_error tls_error in
    run flow m

  let accept
    : Flow.t -> Resolver.t -> server -> (unit, error) result IO.t
    = fun flow resolver server ->
      let ctx = Sendmail_with_starttls.Context_with_tls.make () in
      let m = SMTP.m_relay_init ctx server.info in
      run flow m >>? function
      | `Quit ->
        properly_close_tls flow ctx >>? fun () -> IO.return (Ok ())
      | `Submission { SMTP.domain_from; from; recipients; _ } ->
        recipients_are_reachable ~ipv4:server.info.ipv4 resolver (List.map fst recipients) >>= function
        | true ->
          let id = succ server in
          let key = Messaged.v ~domain_from ~from ~recipients id in
          Md.push server.messaged key >>= fun producer ->
          let m = SMTP.m_mail ctx in
          run flow m >>? fun () ->
          receive_mail
            ~limit:(Int64.to_int server.info.size) flow ctx
            SMTP.(fun ctx -> Monad.recv ctx Value.Payload)
            producer >>? fun () ->
          let m = SMTP.m_end ctx in
          run flow m >>? fun `Quit ->
          properly_close_tls flow ctx >>? fun () -> IO.return (Ok ())
        | false ->
          let e = `Invalid_recipients in
          let m = SMTP.m_properly_close_and_fail ctx ~message:"No valid recipients" e in
          run flow m
end
