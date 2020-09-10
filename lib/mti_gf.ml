open Rresult
open Ptt_tuyau.Lwt_backend
open Lwt.Infix

module Make
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Resolver : Ptt.Sigs.RESOLVER with type +'a io = 'a Lwt.t)
    (StackV4 : Mirage_stack.V4)
= struct
  include Ptt_tuyau.Make(StackV4)

  let src = Logs.Src.create "mti-gf"
  module Log = ((val Logs.src_log src) : Logs.LOG)

  module Random = struct
    type g = Random.g
    type +'a io = 'a Lwt.t

    let generate ?g buf =
      let len = Bytes.length buf in
      let raw = Random.generate ?g len in
      Cstruct.blit_to_bytes raw 0 buf 0 len ;
      Lwt.return ()
  end

  module Relay = Ptt.Relay.Make(Lwt_scheduler)(Lwt_io)(Flow)(Resolver)(Random)

  include Ptt_transmit.Make(Pclock)(StackV4)(Relay.Md)

  let smtp_relay_service resolver conf conf_server =
    let module Server = (val Conduit_mirage.Service.impl TCP.service) in
    Conduit_mirage.Service.init conf ~service:TCP.service >>? fun t ->
    let module Flow = (val Conduit_mirage.impl TCP.protocol) in
    let handle ip port flow () =
      Lwt.catch
        (fun () -> Relay.accept flow resolver conf_server
          >|= R.reword_error (R.msgf "%a" Relay.pp_error) >>= fun res ->
          Flow.close flow >>= function
          | Error err -> Lwt.return (R.error_msgf "%a" Flow.pp_error err)
          | Ok () -> Lwt.return res)
        (function Failure err -> Lwt.return (R.error_msg err)
                | exn -> Lwt.return (Error (`Exn exn))) >>= function
      | Ok () ->
        Log.info (fun m -> m "<%a:%d> submitted a message" Ipaddr.V4.pp ip port) ;
        Lwt.return ()
      | Error (`Msg err) ->
        Log.err (fun m -> m "<%a:%d> %s" Ipaddr.V4.pp ip port err) ;
        Lwt.return ()
      | Error (`Exn exn) ->
        Log.err (fun m -> m "<%a:%d> raised an unknown exception: %s"
                    Ipaddr.V4.pp ip port (Printexc.to_string exn)) ;
        Lwt.return () in
    let rec go () =
      Server.accept t >>? fun flow ->
      let ip, port = TCP.dst flow in
      Lwt.async (handle ip port flow) ; Lwt.pause () >>= go in
    go () >|= R.reword_error (R.msgf "%a" Server.pp_error)

  let smtp_relay_service resolver conf conf_server =
    smtp_relay_service resolver conf conf_server >>= function
    | Ok () -> Lwt.return ()
    | Error err ->
      Log.err (fun m -> m "%a" Conduit_mirage.pp_error err) ;
      Lwt.return ()

  let smtp_logic ~info stack resolver messaged map =
    let rec go () =
      Relay.Md.await messaged >>= fun () ->
      Relay.Md.pop messaged >>= function
      | None -> Lwt.pause () >>= go
      | Some ((key, _, _) as v) ->
        let transmit () =
          Relay.resolve_recipients ~domain:info.Ptt.SSMTP.domain resolver map
            (List.map fst (Ptt.Messaged.recipients key)) >>= fun recipients ->
          transmit ~info stack v recipients in
        Lwt.async transmit ; Lwt.pause () >>= go in
    go ()

  let fiber stack resolver conf map info =
    let conf_server = Relay.create ~info in
    let messaged = Relay.messaged conf_server in
    Lwt.join [ smtp_relay_service resolver conf conf_server
             ; smtp_logic ~info stack resolver messaged map ]
end
