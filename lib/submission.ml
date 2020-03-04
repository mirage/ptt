open Sigs
open Rresult

module Make
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
    (Flow : FLOW with type 'a s = 'a IO.t)
    (Resolver : RESOLVER with type 'a s = 'a IO.t)
    (Random : RANDOM with type 'a s = 'a IO.t)
= struct
  include Common.Make (Scheduler) (IO) (Flow) (Resolver) (Random)
  module Md = Messaged.Make(Scheduler)(IO)

  type 'k server =
    { info : info
    ; messaged : Md.t
    ; mechanisms : Mechanism.t list
    ; authenticator : (Scheduler.t, 'k) Authentication.t
    ; mutable count : int64 }
  and info = SSMTP.info =
    { domain : [ `host ] Domain_name.t
    ; ipv4 : Ipaddr.V4.t
    ; tls : Tls.Config.server
    ; zone : Mrmime.Date.Zone.t
    ; size : int64 }

  let info { info; _ } = info

  let create ~info ~authenticator mechanisms =
    { info
    ; messaged= Md.create ()
    ; mechanisms
    ; authenticator
    ; count= 0L }

  let messaged { messaged; _ } = messaged

  let succ server =
    let v = server.count in
    server.count <- Int64.succ server.count ; v

  let src = Logs.Src.create "ptt-submission"
  module Log = (val Logs.src_log src : Logs.LOG)

  type error =
    [ `Error of [ SSMTP.error
                | `Invalid_recipients
                | `Too_many_tries ]
    | `Connection_close
    | `Too_big_data ]

  let pp_error ppf = function
    | `Error (#SSMTP.error as err) -> SSMTP.pp_error ppf err
    | `Error `Invalid_recipients -> Fmt.pf ppf "Invalid recipients"
    | `Error `Too_many_tries -> Fmt.pf ppf "Too many tries"
    | `Connection_close -> Fmt.pf ppf "Connection close"
    | `Too_big_data -> Fmt.pf ppf "Too big data"

  let authentication ctx ~domain_from flow random hash server mechanism =
    let rec go limit m =
      if limit >= 3
      then
        let e = `Too_many_tries in
        let m = SSMTP.m_properly_close_and_fail ctx ~message:"Too many tries" e in
        run flow m
      else match m with
      | Mechanism.PLAIN ->
        let stamp = Bytes.create 0x10 in
        generate ~g:random stamp >>= fun () ->
        let stamp = Bytes.unsafe_to_string stamp in
        let m =
          let open SSMTP in
          let open Monad in
          send ctx Value.TP_354 [ Base64.encode_string stamp ] >>= fun () ->
          recv ctx Value.Payload in
        run flow m >>? fun v ->
        Logs.debug (fun m -> m "Got a payload while authentication: %S" v) ;
        Authentication.decode_authentication scheduler hash (Authentication.PLAIN (Some stamp)) server.authenticator v
        |> Scheduler.prj >>= function
        | Ok true ->
          let m = SSMTP.(Monad.send ctx Value.PP_235 [ "Accepted, buddy!" ]) in
          run flow m >>? fun () -> IO.return (Ok `Authenticated)
        | (Error _ | Ok false) as res ->
          let () = match res with
            | Error (`Msg err) -> Log.err (fun m -> m "Got an authentication error: %s" err)
            | _ -> () in
          let m =
            let open SSMTP in
            let open Monad in
            let* () = send ctx Value.PN_535 [ "Bad authentication, buddy!" ] in
            SSMTP.m_submission ctx ~domain_from server.mechanisms in
          run flow m >>? function
          | `Quit -> IO.return (Ok `Quit)
          | `Authentication (_domain_from, m) ->
            (* assert (_domain_from = domain_from) ; *)
            go (limit + 1) m in
    go 1 mechanism

  let accept
    : Flow.t -> Resolver.t -> Random.g -> 'k Digestif.hash -> 'k server -> (unit, error) result IO.t
    = fun flow resolver random hash server ->
      let ctx = Colombe.State.Context.make () in
      let m = SSMTP.m_submission_init ctx server.info server.mechanisms in
      run flow m >>? function
      | `Quit -> IO.return (Ok ())
      | `Authentication (domain_from, m) ->
        authentication ctx ~domain_from flow random hash server m >>? function
        | `Quit -> IO.return (Ok ())
        | `Authenticated ->
          let m = SSMTP.m_relay ctx ~domain_from in
          run flow m >>? function
          | `Quit -> IO.return (Ok ())
          | `Submission { domain_from; from; recipients; _ } ->
            recipients_are_reachable ~ipv4:server.info.ipv4 resolver (List.map fst recipients) >>= function
            | true ->
              let id = succ server in
              let key = Messaged.v ~domain_from ~from ~recipients id in
              Md.push server.messaged key >>= fun producer ->
              let m = SSMTP.m_mail ctx in
              run flow m >>? fun () ->
              receive_mail ~limit:(Int64.to_int server.info.size) flow ctx SSMTP.(fun ctx -> Monad.recv ctx Value.Payload) producer >>? fun () ->
              let m = SSMTP.m_end ctx in
              run flow m >>? fun `Quit ->
              IO.return (Ok ())
            | false ->
              let e = `Invalid_recipients in
              let m = SSMTP.m_properly_close_and_fail ctx ~message:"No valid recipients" e in
              run flow m
end
