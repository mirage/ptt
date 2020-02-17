open Rresult
open Ptt_tuyau.Lwt_backend
open Lwt.Infix

let ( <.> ) f g = fun x -> f (g x)

module Make
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Resolver : Ptt.Sigs.RESOLVER with type +'a s = 'a Lwt.t)
    (StackV4 : Mirage_stack.V4)
= struct
  module TLS = Tuyau_mirage_tls
  include Ptt_tuyau.Make(StackV4)

  let tls_endpoint, tls_protocol = TLS.protocol_with_tls ~key:TCP.endpoint TCP.protocol
  let tls_configuration, tls_service = TLS.service_with_tls ~key:TCP.configuration TCP.service tls_protocol

  let src = Logs.Src.create "lipap"
  module Log = ((val Logs.src_log src) : Logs.LOG)

  module Random = struct
    type g = Random.g
    type +'a s = 'a Lwt.t

    let generate ?g buf =
      let len = Bytes.length buf in
      let raw = Random.generate ?g len in
      Cstruct.blit_to_bytes raw 0 buf 0 len ;
      Lwt.return ()
  end

  module Tls_flow = (val (flow (Tuyau_mirage.impl_of_flow tls_protocol)))
  module Submission = Ptt.Submission.Make(Lwt_scheduler)(Lwt_io)(Tls_flow)(Resolver)(Random)

  include Ptt_transmit.Make(Pclock)(StackV4)(Submission.Md)

  let smtp_submission_service resolver random hash conf conf_server =
    let tls = (Submission.info conf_server).Ptt.SSMTP.tls in
    Tuyau_mirage.impl_of_service ~key:tls_endpoint tls_service |> Lwt.return >>? fun (module Server) ->
    Tuyau_mirage.serve ~key:tls_configuration (conf, tls) ~service:tls_service >>? fun (t, protocol) ->
    let module Flow = (val (Tuyau_mirage.impl_of_flow protocol)) in
    let handle ip port flow () =
      Lwt.catch
        (fun () -> Submission.accept flow resolver random hash conf_server
          >|= R.reword_error (R.msgf "%a" Submission.pp_error) >>= fun res ->
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
      let ip, port = (TCP.dst <.> TLS.underlying) flow in
      Lwt.async (handle ip port flow) ; Lwt.pause () >>= go in
    go () >|= R.reword_error (R.msgf "%a" Server.pp_error)

  let smtp_submission_service resolver random hash conf conf_server =
    smtp_submission_service resolver random hash conf conf_server >>= function
    | Ok () -> Lwt.return ()
    | Error err ->
      Log.err (fun m -> m "%a" Tuyau_mirage.pp_error err) ;
      Lwt.return ()

  let smtp_logic ~info stack resolver messaged map =
    let rec go () =
      Submission.Md.await messaged >>= fun () ->
      Submission.Md.pop messaged >>= function
      | None -> Lwt.pause () >>= go
      | Some ((key, _, _) as v) ->
        let transmit () =
          Submission.resolve_recipients ~domain:info.Ptt.SSMTP.domain resolver map
            (List.map fst (Ptt.Messaged.recipients key)) >>= fun recipients ->
          transmit ~info stack v recipients in
        Lwt.async transmit ; Lwt.pause () >>= go in
    go ()

  let fiber stack resolver random hash conf map info authenticator mechanisms =
    let conf_server = Submission.create ~info ~authenticator mechanisms in
    let messaged = Submission.messaged conf_server in
    Lwt.join [ smtp_submission_service resolver random hash conf conf_server
             ; smtp_logic ~info stack resolver messaged map ]
end
