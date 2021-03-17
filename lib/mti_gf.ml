open Rresult
open Ptt_tuyau.Lwt_backend
open Lwt.Infix

module Make
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Resolver : Ptt.Sigs.RESOLVER with type +'a io = 'a Lwt.t)
    (Stack : Mirage_stack.V4V6) =
struct
  include Ptt_tuyau.Make (Stack)

  let src = Logs.Src.create "mti-gf"

  module Log : Logs.LOG = (val Logs.src_log src)

  module Random = struct
    type g = Random.g
    type +'a io = 'a Lwt.t

    let generate ?g buf =
      let len = Bytes.length buf in
      let raw = Random.generate ?g len in
      Cstruct.blit_to_bytes raw 0 buf 0 len
      ; Lwt.return ()
  end

  module Relay =
    Ptt.Relay.Make (Lwt_scheduler) (Lwt_io) (Flow) (Resolver) (Random)

  module Server = Ptt_tuyau.Server (Time) (Stack)
  include Ptt_transmit.Make (Pclock) (Stack) (Relay.Md)

  let smtp_relay_service ~port stack resolver conf_server =
    Server.init ~port stack >>= fun service ->
    let handler flow =
      let ip, port = Stack.TCP.dst flow in
      let v = Flow.make flow in
      Lwt.catch
        (fun () ->
          Relay.accept v resolver conf_server
          >|= R.reword_error (R.msgf "%a" Relay.pp_error)
          >>= fun res ->
          Stack.TCP.close flow >>= fun () -> Lwt.return res)
        (function
          | Failure err -> Lwt.return (R.error_msg err)
          | exn -> Lwt.return (Error (`Exn exn)))
      >>= function
      | Ok () ->
        Log.info (fun m -> m "<%a:%d> submitted a message" Ipaddr.pp ip port)
        ; Lwt.return ()
      | Error (`Msg err) ->
        Log.err (fun m -> m "<%a:%d> %s" Ipaddr.pp ip port err)
        ; Lwt.return ()
      | Error (`Exn exn) ->
        Log.err (fun m ->
            m "<%a:%d> raised an unknown exception: %s" Ipaddr.pp ip port
              (Printexc.to_string exn))
        ; Lwt.return () in
    let (`Initialized fiber) = Server.serve_when_ready ~handler service in
    fiber

  let smtp_logic ~info stack resolver messaged map =
    let rec go () =
      Relay.Md.await messaged >>= fun () ->
      Relay.Md.pop messaged >>= function
      | None -> Lwt.pause () >>= go
      | Some ((key, _, _) as v) ->
        let transmit () =
          Relay.resolve_recipients ~domain:info.Ptt.SSMTP.domain resolver map
            (List.map fst (Ptt.Messaged.recipients key))
          >>= fun recipients -> transmit ~info stack v recipients in
        Lwt.async transmit
        ; Lwt.pause () >>= go in
    go ()

  let fiber ~port stack resolver map info =
    let conf_server = Relay.create ~info in
    let messaged = Relay.messaged conf_server in
    Lwt.join
      [
        smtp_relay_service ~port stack resolver conf_server
      ; smtp_logic ~info stack resolver messaged map
      ]
end
