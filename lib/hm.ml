open Rresult
open Ptt_tuyau.Lwt_backend
open Lwt.Infix

let src = Logs.Src.create "ptt.hm"

module Log : Logs.LOG = (val Logs.src_log src)

module Make
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Resolver : Ptt.Sigs.RESOLVER with type +'a io = 'a Lwt.t)
    (Stack : Tcpip.Stack.V4V6)
    (DNS : Dns_client_mirage.S
             with type Transport.stack = Stack.t
              and type 'a Transport.io = 'a Lwt.t) =
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

  module Verifier =
    Ptt.Relay.Make (Lwt_scheduler) (Lwt_io) (Flow) (Resolver) (Random)

  module Server = Ptt_tuyau.Server (Time) (Stack)
  include Ptt_transmit.Make (Pclock) (Stack) (Verifier.Md)
  module Lwt_scheduler = Uspf.Sigs.Make (Lwt)

  module Uspf_dns = struct
    type t = DNS.t
    type backend = Lwt_scheduler.t

    type error =
      [ `Msg of string
      | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
      | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ]

    let getrrecord dns key domain_name =
      Lwt_scheduler.inj @@ DNS.get_resource_record dns key domain_name
  end

  let smtp_verifier_service ~pool ?stop ~port stack resolver conf_server =
    Server.init ~port stack >>= fun service ->
    let handler pool flow =
      let ip, port = Stack.TCP.dst flow in
      let v = Flow.make flow in
      Lwt.catch
        (fun () ->
          Lwt_pool.use pool @@ fun (encoder, decoder, queue) ->
          Verifier.accept ~encoder:(Fun.const encoder)
            ~decoder:(Fun.const decoder) ~queue:(Fun.const queue) ~ipaddr:ip v
            resolver conf_server
          >|= R.reword_error (R.msgf "%a" Verifier.pp_error)
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

  let state =
    let open Uspf.Sigs in
    let open Lwt_scheduler in
    {
      return= (fun x -> inj (Lwt.return x))
    ; bind= (fun x f -> inj (prj x >>= fun x -> prj (f x)))
    }

  let stream_of_list lst =
    let lst = ref lst in
    fun () ->
      match !lst with
      | [] -> Lwt.return_none
      | str :: rest ->
        lst := rest
        ; Lwt.return_some (str, 0, String.length str)

  let stream_of_field (field_name : Mrmime.Field_name.t) unstrctrd =
    stream_of_list
      [
        (field_name :> string); ": "; Unstrctrd.to_utf_8_string unstrctrd; "\r\n"
      ]

  let concat_stream a b =
    let current = ref a in
    let rec next () =
      let v = !current () in
      v >>= function
      | Some _ -> v
      | None ->
        if !current == b then Lwt.return_none
        else (
          current := b
          ; next ()) in
    next

  let smtp_logic ~pool ~info ~tls stack resolver messaged map dns =
    let rec go () =
      Verifier.Md.await messaged >>= fun () ->
      Verifier.Md.pop messaged >>= function
      | None -> Lwt.pause () >>= go
      | Some (key, queue, consumer) ->
        Log.debug (fun m -> m "Got an email.")
        ; let verify_and_transmit () =
            Verifier.resolve_recipients ~domain:info.Ptt.SSMTP.domain resolver
              map
              (List.map fst (Ptt.Messaged.recipients key))
            >>= fun recipients ->
            let sender, _ = Ptt.Messaged.from key in
            let ctx =
              Uspf.empty |> Uspf.with_ip (Ptt.Messaged.ipaddr key) |> fun ctx ->
              Option.fold ~none:ctx
                ~some:(fun sender -> Uspf.with_sender (`MAILFROM sender) ctx)
                sender in
            Uspf.get ~ctx state dns (module Uspf_dns) |> Lwt_scheduler.prj
            >>= function
            | Error (`Msg err) ->
              Log.err (fun m -> m "Got an error from the SPF verifier: %s." err)
              ; (* TODO(dinosaure): save this result into the incoming email. *)
                transmit ~pool ~info ~tls stack (key, queue, consumer)
                  recipients
            | Ok record ->
              Uspf.check ~ctx state dns (module Uspf_dns) record
              |> Lwt_scheduler.prj
              >>= fun res ->
              let receiver =
                `Domain (Domain_name.to_strings info.Ptt.SSMTP.domain) in
              let field_name, unstrctrd = Uspf.to_field ~ctx ~receiver res in
              let stream = stream_of_field field_name unstrctrd in
              let consumer = concat_stream stream consumer in
              transmit ~pool ~info ~tls stack (key, queue, consumer) recipients
          in
          Lwt.async verify_and_transmit
          ; Lwt.pause () >>= go in
    go ()

  let fiber ?(limit = 20) ?stop ?locals ~port ~tls stack resolver info dns =
    let conf_server = Verifier.create ~info in
    let messaged = Verifier.messaged conf_server in
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
        smtp_verifier_service ~pool:pool0 ?stop ~port stack resolver conf_server
      ; smtp_logic ~pool:pool1 ~info ~tls stack resolver messaged locals dns
      ]
end
