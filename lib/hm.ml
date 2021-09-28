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

  module Dns = struct
    include Dns_client_mirage.Make (Random) (Time) (Mclock) (Stack)

    type +'a io = 'a Lwt.t

    type error =
      [ `Msg of string
      | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
      | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ]

    let getrrecord dns key domain_name = get_resource_record dns key domain_name
  end

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

  module DMARC = Dmarc_lwt.Make (Dns)
  module Server = Ptt_tuyau.Server (Time) (Stack)
  include Ptt_transmit.Make (Pclock) (Stack) (Relay.Md)

  let smtp_verifier_service ?stop ~port stack resolver conf_server =
    Server.init ~port stack >>= fun service ->
    let handler flow =
      let ip, port = Stack.TCP.dst flow in
      let v = Flow.make flow in
      Lwt.catch
        (fun () ->
          Relay.accept ~ipaddr:ip v resolver conf_server
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
    let (`Initialized fiber) = Server.serve_when_ready ?stop ~handler service in
    fiber

  let epoch () =
    Int64.of_float (Ptime.to_float_s (Ptime.v (Pclock.now_d_ps ())))

  let smtp_logic ~info:_ ~tls:_ stack _resolver messaged _map =
    let dns = Dns.create stack in
    let rec go () =
      Relay.Md.await messaged >>= fun () ->
      Relay.Md.pop messaged >>= function
      | None -> Lwt.pause () >>= go
      | Some (key, _queue, consumer) ->
        Log.debug (fun m -> m "Got an email.")
        ; let verify_and_transmit () =
            let sender, _ = Ptt.Messaged.from key in
            let ctx =
              Spf.empty |> Spf.with_ip (Ptt.Messaged.ipaddr key) |> fun ctx ->
              Option.fold ~none:ctx
                ~some:(fun sender -> Spf.with_sender (`MAILFROM sender) ctx)
                sender in
            DMARC.verify ~newline:Dmarc.CRLF ~ctx ~epoch dns consumer
            >>= function
            | Ok _ -> assert false
            | Error _ -> assert false in
          Lwt.async verify_and_transmit
          ; Lwt.pause () >>= go in
    go ()

  let fiber ?stop ~port ~tls stack resolver map info =
    let conf_server = Relay.create ~info in
    let messaged = Relay.messaged conf_server in
    Lwt.join
      [
        smtp_verifier_service ?stop ~port stack resolver conf_server
      ; smtp_logic ~info ~tls stack resolver messaged map
      ]
end
