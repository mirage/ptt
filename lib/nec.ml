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

  let src = Logs.Src.create "nec"

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

  module Signer =
    Ptt.Relay.Make (Lwt_scheduler) (Lwt_io) (Flow) (Resolver) (Random)
  (* XXX(dinosaure): the [signer] is a simple relay. *)

  module Server = Ptt_tuyau.Server (Time) (Stack)
  include Ptt_transmit.Make (Pclock) (Stack) (Signer.Md)

  let smtp_signer_service ?stop ~port stack resolver conf_server =
    Server.init ~port stack >>= fun service ->
    let handler flow =
      let ip, port = Stack.TCP.dst flow in
      let v = Flow.make flow in
      Lwt.catch
        (fun () ->
          Signer.accept ~ipaddr:ip v resolver conf_server
          >|= R.reword_error (R.msgf "%a" Signer.pp_error)
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
    let (`Initialized fiber) = Server.serve_when_ready ?stop ~handler service in
    fiber

  let smtp_logic ~info ~tls stack resolver messaged (private_key, dkim) map =
    let rec go () =
      Signer.Md.await messaged >>= fun () ->
      Signer.Md.pop messaged >>= function
      | None -> Lwt.pause () >>= go
      | Some (key, queue, consumer) ->
        Log.debug (fun m -> m "Got an email.")
        ; let sign_and_transmit () =
            Lwt.catch (fun () ->
                Dkim_mirage.sign ~key:private_key ~newline:Dkim.CRLF consumer
                  dkim
                >>= fun (_dkim', consumer') ->
                Log.debug (fun m -> m "Incoming email signed.")
                ; Signer.resolve_recipients ~domain:info.Ptt.SSMTP.domain
                    resolver map
                    (List.map fst (Ptt.Messaged.recipients key))
                  >>= fun recipients ->
                  Log.debug (fun m ->
                      m "Send the signed email to the destination.")
                  ; transmit ~info ~tls stack (key, queue, consumer') recipients)
            @@ fun _exn ->
            Log.err (fun m -> m "Impossible to sign the incoming email.")
            ; Lwt.return_unit in
          Lwt.async sign_and_transmit
          ; Lwt.pause () >>= go in
    go ()

  let fiber ?stop ~port ~tls stack resolver (private_key, dkim) map info =
    let conf_server = Signer.create ~info in
    let messaged = Signer.messaged conf_server in
    Lwt.join
      [
        smtp_signer_service ?stop ~port stack resolver conf_server
      ; smtp_logic ~info ~tls stack resolver messaged (private_key, dkim) map
      ]
end
