module Lwt_backend = Lwt_backend

let src = Logs.Src.create "ptt.tuyau"

module Log = (val Logs.src_log src)

module type FLOW = Ptt.Sigs.FLOW with type +'a io = 'a Lwt.t

module Client (Stack : Tcpip.Stack.V4V6) = struct
  open Rresult
  open Lwt.Infix
  open Lwt_backend
  module Flow = Rdwr.Make (Stack.TCP)

  let rdwr : (Flow.t, Lwt_scheduler.t) Colombe.Sigs.rdwr =
    let rd flow buf off len =
      Lwt_scheduler.inj
      @@ (Flow.recv flow buf off len >>= function
          | 0 -> Lwt.return `End
          | len -> Lwt.return (`Len len)) in
    let wr flow buf off len = Lwt_scheduler.inj (Flow.send flow buf off len) in
    {Colombe.Sigs.rd; Colombe.Sigs.wr}

  let sendmail
      ?encoder
      ?decoder
      ?queue
      ~info
      ~tls
      stack
      mx_ipaddr
      emitter
      producer
      recipients =
    Stack.TCP.create_connection stack (mx_ipaddr, 25)
    >|= R.reword_error (fun err -> `Flow err)
    >>? fun flow ->
    let flow' = Flow.make flow in
    let ctx =
      Sendmail_with_starttls.Context_with_tls.make ?encoder ?decoder ?queue ()
    in
    let domain =
      let vs = Domain_name.to_strings info.Ptt.Logic.domain in
      Colombe.Domain.Domain vs in
    Lwt.catch
      (fun () ->
        Sendmail_with_starttls.sendmail lwt rdwr flow' ctx tls ~domain emitter
          recipients producer
        |> Lwt_scheduler.prj
        >|= R.reword_error (fun err -> `Sendmail err))
      (function
        | Failure err ->
          Lwt.return (R.error_msg err)
          (* XXX(dinosaure): should come from [rdwr]. *)
        | exn -> Lwt.return (Error (`Exn exn)))
    >>= fun res ->
    Stack.TCP.close flow >>= fun () ->
    match res with
    | Ok () -> Lwt.return (Ok ())
    | Error (`Sendmail `STARTTLS_unavailable) ->
      Lwt.return_error `STARTTLS_unavailable
    | Error (`Sendmail err) ->
      Lwt.return (R.error_msgf "%a" Sendmail_with_starttls.pp_error err)
    | Error (`Msg _) as err -> Lwt.return err
    | Error (`Exn exn) ->
      Lwt.return (R.error_msgf "Unknown error: %s" (Printexc.to_string exn))

  let sendmail_without_tls
      ?encoder ?decoder ~info stack mx_ipaddr emitter producer recipients =
    Stack.TCP.create_connection stack (mx_ipaddr, 25)
    >|= R.reword_error (fun err -> `Flow err)
    >>? fun flow ->
    let flow' = Flow.make flow in
    let ctx = Colombe.State.Context.make ?encoder ?decoder () in
    let domain =
      let vs = Domain_name.to_strings info.Ptt.Logic.domain in
      Colombe.Domain.Domain vs in
    Lwt.catch
      (fun () ->
        Sendmail.sendmail lwt rdwr flow' ctx ~domain emitter recipients producer
        |> Lwt_scheduler.prj
        >|= R.reword_error (fun err -> `Sendmail err))
      (function
        | Failure err -> Lwt.return (R.error_msg err)
        | exn -> Lwt.return (Error (`Exn exn)))
    >>= fun res ->
    Stack.TCP.close flow >>= fun () ->
    match res with
    | Ok () -> Lwt.return (Ok ())
    | Error (`Sendmail err) ->
      Lwt.return (R.error_msgf "%a" Sendmail.pp_error err)
    | Error (`Msg _) as err -> Lwt.return err
    | Error (`Exn exn) ->
      Lwt.return (R.error_msgf "Unknown error: %s" (Printexc.to_string exn))

  let pp_error ppf = function
    | `Msg err -> Fmt.string ppf err
    | `Flow err -> Stack.TCP.pp_error ppf err
    | `STARTTLS_unavailable -> Fmt.string ppf "STARTTLS unavailable"
end

module Server (Time : Mirage_time.S) (Stack : Tcpip.Stack.V4V6) = struct
  open Lwt.Infix

  type service = {
      stack: Stack.TCP.t
    ; queue: Stack.TCP.flow Queue.t
    ; condition: unit Lwt_condition.t
    ; mutex: Lwt_mutex.t
    ; mutable closed: bool
  }

  let init ~port stack =
    let queue = Queue.create () in
    let condition = Lwt_condition.create () in
    let mutex = Lwt_mutex.create () in

    let listener flow =
      Lwt_mutex.lock mutex >>= fun () ->
      Queue.push flow queue
      ; Lwt_condition.signal condition ()
      ; Lwt_mutex.unlock mutex
      ; Lwt.return_unit in
    Stack.TCP.listen ~port stack listener
    ; Lwt.return {stack; queue; condition; mutex; closed= false}

  let rec accept ({queue; condition; mutex; _} as t) =
    Lwt_mutex.lock mutex >>= fun () ->
    let rec await () =
      if Queue.is_empty queue && not t.closed then
        Lwt_condition.wait condition ~mutex >>= await
      else Lwt.return_unit in
    await () >>= fun () ->
    match Queue.pop queue with
    | flow -> Lwt_mutex.unlock mutex ; Lwt.return_ok flow
    | exception Queue.Empty ->
      if t.closed then (Lwt_mutex.unlock mutex ; Lwt.return_error `Closed)
      else (Lwt_mutex.unlock mutex ; accept t)

  let close ({stack; condition; _} as t) =
    t.closed <- true
    ; Stack.TCP.disconnect stack >>= fun () ->
      Lwt_condition.signal condition ()
      ; Lwt.return_unit

  let serve_when_ready ?stop ~handler service =
    `Initialized
      (let switched_off =
         let t, u = Lwt.wait () in
         Lwt_switch.add_hook stop (fun () ->
             Lwt.wakeup_later u (Ok `Stopped)
             ; Lwt.return_unit)
         ; t in
       let rec loop () =
         accept service >>= function
         | Ok flow ->
           Lwt.async (fun () -> handler flow)
           ; loop ()
         | Error `Closed -> Lwt.return_error `Closed
         | Error _ -> Lwt.pause () >>= loop in
       let stop_result =
         Lwt.pick [switched_off; loop ()] >>= function
         | Ok `Stopped -> close service >>= fun () -> Lwt.return_ok ()
         | Error _ as err -> close service >>= fun () -> Lwt.return err in
       stop_result >>= function Ok () | Error `Closed -> Lwt.return_unit)
end
