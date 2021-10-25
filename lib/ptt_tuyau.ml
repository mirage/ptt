module Lwt_backend = Lwt_backend

module type FLOW = Ptt.Sigs.FLOW with type +'a io = 'a Lwt.t

module Make (Stack : Mirage_stack.V4V6) = struct
  open Rresult
  open Lwt.Infix
  open Lwt_backend
  module Flow = Unixiz.Make (Stack.TCP)

  module TLSFlow = struct
    module Flow = Tls_mirage.Make (Stack.TCP)
    include Unixiz.Make (Flow)

    let failwith pp = function
      | Ok v -> Lwt.return v
      | Error err -> Lwt.fail (Failure (Fmt.str "%a" pp err))

    let server stack cfg =
      Flow.server_of_flow cfg stack >>= failwith Flow.pp_write_error >|= make

    let client stack cfg =
      Flow.client_of_flow cfg stack >>= failwith Flow.pp_write_error >|= make
  end

  let rdwr : (Flow.t, Lwt_scheduler.t) Colombe.Sigs.rdwr =
    let rd flow buf off len =
      Lwt_scheduler.inj
      @@ (Flow.recv flow buf off len >>= function
          | 0 -> Lwt.return `End
          | len -> Lwt.return (`Len len)) in
    let wr flow buf off len = Lwt_scheduler.inj (Flow.send flow buf off len) in
    {Colombe.Sigs.rd; Colombe.Sigs.wr}

  let sendmail ~info ~tls stack mx_ipaddr emitter producer recipients =
    let tcp = Stack.tcp stack in
    Stack.TCP.create_connection tcp (mx_ipaddr, 25)
    >|= R.reword_error (fun err -> `Flow err)
    >>? fun flow ->
    let flow' = Flow.make flow in
    let ctx = Sendmail_with_starttls.Context_with_tls.make () in
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

  let sendmail_without_tls ~info stack mx_ipaddr emitter producer recipients =
    let tcp = Stack.tcp stack in
    Stack.TCP.create_connection tcp (mx_ipaddr, 25)
    >|= R.reword_error (fun err -> `Flow err)
    >>? fun flow ->
    let flow' = Flow.make flow in
    let ctx = Colombe.State.Context.make () in
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

module Server (Time : Mirage_time.S) (Stack : Mirage_stack.V4V6) = struct
  open Lwt.Infix

  type service = {
      stack: Stack.t
    ; consumer: Stack.TCP.flow Lwt_stream.t
    ; producer: Stack.TCP.flow Lwt_stream.bounded_push
  }

  let init ~port stack =
    let consumer, producer = Lwt_stream.create_bounded 10 in
    let listener flow = producer#push flow in
    Stack.listen_tcp ~port stack listener
    ; Lwt.return {stack; consumer; producer}

  let rec accept ({consumer; producer; _} as t) =
    Lwt_stream.get consumer >>= function
    | Some flow -> Lwt.return_ok flow
    | None when producer#closed -> Lwt.return_error `Closed
    | None -> accept t

  let close {stack; producer; _} =
    Stack.disconnect stack >>= fun () -> producer#close ; Lwt.return_unit

  let ( >>? ) = Lwt_result.bind

  let serve_when_ready ?timeout ?stop ~handler service =
    let timeout () =
      match timeout with
      | None -> Lwt.wait () |> fst
      | Some t -> Time.sleep_ns t in
    `Initialized
      (let switched_off =
         let t, u = Lwt.wait () in
         Lwt_switch.add_hook stop (fun () ->
             Lwt.wakeup_later u (Ok `Stopped)
             ; Lwt.return_unit)
         ; t in
       let rec loop () =
         let accept =
           accept service >>? fun flow -> Lwt.return_ok (`Flow flow) in
         Lwt.pick [accept; (timeout () >|= fun () -> Ok `Timeout)] >>? function
         | `Flow flow ->
           Lwt.async (fun () -> handler flow)
           ; Lwt.pause () >>= loop
         | `Timeout -> Lwt.return_ok `Timeout in
       let stop_result =
         Lwt.pick [switched_off; loop ()] >>= function
         | Ok (`Timeout | `Stopped) ->
           close service >>= fun () -> Lwt.return_ok ()
         | Error _ as err -> close service >>= fun () -> Lwt.return err in
       stop_result >>= function Ok () | Error `Closed -> Lwt.return_unit)
end
