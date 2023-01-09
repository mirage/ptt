open Rresult
open Ptt_tuyau.Lwt_backend
open Lwt.Infix

let src = Logs.Src.create "ptt.mti-gf"

module Log : Logs.LOG = (val Logs.src_log src)

module Make
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Resolver : Ptt.Sigs.RESOLVER with type +'a io = 'a Lwt.t)
    (Stack : Tcpip.Stack.V4V6) =
struct
  include Ptt_tuyau.Client (Stack)

  module Random = struct
    type g = Random.g
    type +'a io = 'a Lwt.t

    let generate ?g buf =
      let len = Bytes.length buf in
      let raw = Random.generate ?g len in
      Cstruct.blit_to_bytes raw 0 buf 0 len
      ; Lwt.return ()
  end

  module Flow = Rdwr.Make (Stack.TCP)

  module Relay =
    Ptt.Relay.Make (Lwt_scheduler) (Lwt_io) (Flow) (Resolver) (Random)

  module Server = Ptt_tuyau.Server (Time) (Stack)
  include Ptt_transmit.Make (Pclock) (Stack) (Relay.Md)

  let smtp_relay_service ~pool ?stop ~port stack resolver conf_server =
    Server.init ~port stack >>= fun service ->
    let handler pool flow =
      let ip, port = Stack.TCP.dst flow in
      let v = Flow.make flow in
      Lwt.catch
        (fun () ->
          Lwt_pool.use pool @@ fun (encoder, decoder, queue) ->
          Relay.accept ~encoder:(Fun.const encoder) ~decoder:(Fun.const decoder)
            ~queue:(Fun.const queue) ~ipaddr:ip v resolver conf_server
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
    let (`Initialized fiber) =
      Server.serve_when_ready ?stop ~handler:(handler pool) service in
    fiber

  let filter_on_domain ~domain fwd_path =
    match fwd_path with
    | Colombe.Forward_path.Postmaster -> true
    | Domain (Domain vs) | Forward_path {Colombe.Path.domain= Domain vs; _} ->
      let domain' = Domain_name.(host_exn (of_strings_exn vs)) in
      Domain_name.equal domain domain'
    | Domain _ | Forward_path _ -> false

  let to_reverse_path ~domain = function
    | Colombe.Forward_path.Postmaster ->
      Some
        (Some
           {
             Colombe.Path.local= `String "postmaster"
           ; domain= Domain (Domain_name.to_strings domain)
           ; rest= []
           })
    | Domain _ -> None
    | Forward_path path -> Some (Some path)

  (* NOTE(dinosaure): as the relay, we disturb the route of the email and the
     receiver can fail on the SPF check. Imagine a relay with this association:
     [foo@bar.com] -> [foo@gmail.com]. If a sender ([bar@foo.com]) sends an
     email to [foo@bar.com], our [verifier] will be able to do a SPF
     verification. Afterthat, the [verifier] will send back the incoming email
     (with a new [Received-SPF] field) to our relay. The relay will find that
     the real destination of the email is [foo@gmail.com]. In such situation,
     we will talk to [gmail.com:25] which will do another SPF verification.

     In such situation, the [MAIL FROM] given to [gmail.com:25] must have the
     [bar.com] domain (our domain) to allow [gmail.com:25] to really check SPF
     metadata. However, the initial [MAIL FROM] was [bar@foo.com]!

     We must do a translation of the [MAIL FROM] which must correspond to the
     initial destination ([foo@bar.com]) when we talk to [gmail.com:25]. It
     complexify a bit the situation when:
     1) we must verify that the initial destination ([foo@bar.com]) exists as
        a user into our database
     2) currently, we handles only one case: when we have **one** recipient with
        our domain [bar.com] - mainly because we can have only one sender

     If someone wants to send to [foo@bar.com] and [bar@bar.com], which
     [MAIL FROM] we should use for real destinations? We obviously can associate
     the [MAIL FROM] per destinations ([foo@bar.com] will be the [MAIL FROM] of
     its destination, and the same appear for [bar@bar.com]). However,
     [bar@bar.com] can be associate to [foo@gmail.com]... In that case, we have
     2 users with the same destination and we fallback on the situation where
     we can not really choose between [foo@bar.com] and [bar@bar.com] as the
     expected [MAIL FROM] for [gmail.com:25]...

     So currently, we handle the basic case where someone wants to send an email
     to only one [@bar.com]. *)

  let smtp_logic ~pool ~info ~tls stack resolver messaged map =
    let rec go () =
      Relay.Md.await messaged >>= fun () ->
      Relay.Md.pop messaged >>= function
      | None -> Lwt.pause () >>= go
      | Some ((key, _, _) as v) ->
        let transmit () =
          let recipients = Ptt.Messaged.recipients key in
          let translated_emitters =
            List.map fst recipients
            |> List.find_all (filter_on_domain ~domain:info.Ptt.SSMTP.domain)
            |> List.filter_map (to_reverse_path ~domain:info.Ptt.SSMTP.domain)
          in
          let emitter =
            match translated_emitters, map with
            | [_emitter], None -> None
            | [emitter], Some map ->
              if Ptt.Relay_map.exists emitter map then Some emitter else None
            | [], _ -> None
            | _ :: _, _ ->
              None (* TODO(dinosaure): see the huge comment above. *) in
          Relay.resolve_recipients ~domain:info.Ptt.SSMTP.domain resolver map
            (List.map fst recipients)
          >>= fun recipients ->
          transmit ~pool ~info ~tls ?emitter stack v recipients in
        Lwt.async transmit
        ; Lwt.pause () >>= go in
    go ()

  let fiber ?(limit = 20) ?stop ?locals ~port ~tls stack resolver info =
    let conf_server = Relay.create ~info in
    let messaged = Relay.messaged conf_server in
    let pool0 =
      Lwt_pool.create limit @@ fun () ->
      let encoder = Bytes.create Colombe.Encoder.io_buffer_size in
      let decoder = Bytes.create Colombe.Decoder.io_buffer_size in
      let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
      Lwt.return (encoder, decoder, queue) in
    let pool1 =
      Lwt_pool.create limit @@ fun () ->
      let encoder = Bytes.create Colombe.Encoder.io_buffer_size in
      let decoder = Bytes.create Colombe.Decoder.io_buffer_size in
      let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
      Lwt.return (encoder, decoder, queue) in
    Lwt.join
      [
        smtp_relay_service ~pool:pool0 ?stop ~port stack resolver conf_server
      ; smtp_logic ~pool:pool1 ~info ~tls stack resolver messaged locals
      ]
end
