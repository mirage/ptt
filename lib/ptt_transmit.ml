let ( <.> ) f g x = f (g x)
let icompare : int -> int -> int = fun a b -> compare a b
let src = Logs.Src.create "ptt.transmit"

module Log = (val Logs.src_log src)

module Make
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    (Md : Ptt.Messaged.S with type 'a s = 'a Lwt.t) =
struct
  open Lwt.Infix
  open Ptt_tuyau.Lwt_backend
  include Ptt_tuyau.Client (Stack)

  let local_to_forward_path ~domain:mx_domain local =
    let local =
      `Dot_string (List.map (function `Atom x -> x | `String x -> x) local)
    in
    Colombe.Forward_path.Forward_path
      {
        Colombe.Path.local
      ; Colombe.Path.domain= mx_domain
      ; Colombe.Path.rest= []
      }

  let plug_consumer_to_producers consumer producers =
    let rec go () =
      consumer () >>= function
      | Some ((str, off, len) as v) ->
        Log.debug (fun m -> m "Send to %d recipient(s)" (List.length producers));
        Log.debug (fun m ->
            m "@[<hov>%a@]" (Hxd_string.pp Hxd.default) (String.sub str off len));
        List.iter (fun producer -> producer (Some v)) producers;
        Lwt.pause () >>= go
      | None ->
        Log.debug (fun m ->
            m "Send <End-of-flow> to %d recipient(s)." (List.length producers));
        List.iter (fun producer -> producer None) producers;
        Lwt.return () in
    go

  let ( <+> ) s0 s1 =
    let current = ref s0 in
    let rec next () =
      let tmp = Lwt_scheduler.prj (!current ()) in
      let res =
        tmp >>= function
        | Some _ -> tmp
        | None ->
          if !current == s1 then Lwt.return None
          else (
            current := s1;
            Lwt_scheduler.prj (next ())) in
      Lwt_scheduler.inj res in
    next

  type pool =
    (bytes * bytes * (char, Bigarray.int8_unsigned_elt) Ke.Rke.t) Lwt_pool.t

  let received ~info ~key recipient =
    let id = Ptt.Messaged.id key in
    let by = Domain_name.to_strings info.Ptt.Logic.domain in
    let id =
      let open Mrmime.Mailbox in
      Local.(v [w (Fmt.str "%08LX" id)]), `Domain by in
    let received =
      Received.make
        ~from:(Received.Only (Ptt.Messaged.domain_from key))
        ~by:(Received.Only (Colombe.Domain.Domain by)) ~via:Received.tcp
        ~protocol:Received.esmtp ~id ~zone:info.Ptt.Logic.zone recipient
        (Ptime.v (Pclock.now_d_ps ())) in
    let stream = Prettym.to_stream Received.Encoder.as_field received in
    let stream =
      Lwt_stream.from (Lwt.return <.> stream)
      |> Lwt_stream.map (fun s -> s, 0, String.length s) in
    (* XXX(dinosaure): don't use [Fun.const] here, we need to keep
       the side-effect of the consommation. *)
    Lwt_scheduler.inj <.> fun () -> Lwt_stream.get stream

  let pp_key ppf = function
    | `Ipaddr ipaddr -> Ipaddr.pp ppf ipaddr
    | `Domain (domain, _) -> Domain_name.pp ppf domain

  (* XXX(dinosaure): this function tries to send an email to a recipient.
     It tries multiple MX targets given by the recipient's domain. *)
  let sendmail_to_a_target ~pool ~info ~tls ~key stack emitter (stream, (k, vs))
      =
    let open Colombe in
    let open Forward_path in
    let mx_domain, mxs =
      match k with
      | `Ipaddr (Ipaddr.V4 v4 as mx_ipaddr) ->
        Domain.IPv4 v4, Ptt.Mxs.(singleton (v ~preference:0 mx_ipaddr))
      | `Ipaddr (Ipaddr.V6 v6 as mx_ipaddr) ->
        Domain.IPv6 v6, Ptt.Mxs.(singleton (v ~preference:0 mx_ipaddr))
      | `Domain (mx_domain, mxs) ->
        Domain.Domain (Domain_name.to_strings mx_domain), mxs in
    let recipients =
      match vs with
      | `All -> [Domain mx_domain]
      | `Local vs -> List.map (local_to_forward_path ~domain:mx_domain) vs in
    let received =
      match recipients with
      | [Forward_path path] -> received ~info ~key (Some path)
      | _ -> received ~info ~key None in
    let stream = received <+> stream in
    (* TODO(dinosaure): [sendmail] and [sendmail_without_tls] can consume
       the stream and fail. In that case, all sub-sequent call to these
       functions will try to send an empty email. We must copy the stream
       for each call of [sendmail] and [sendmail_without_tls]. *)
    let rec go = function
      | [] ->
        Log.err (fun m ->
            m "Impossible to send an email to %a (no solution found)." pp_key k);
        Lwt.return ()
      | {Ptt.Mxs.mx_ipaddr; _} :: rest -> (
        Log.debug (fun m ->
            m "Transmit the incoming email to %a (%a)." Ipaddr.pp mx_ipaddr
              Domain.pp mx_domain);
        Lwt_pool.use pool (fun (encoder, decoder, queue) ->
            sendmail ~encoder:(Fun.const encoder) ~decoder:(Fun.const decoder)
              ~queue:(Fun.const queue) ~info ~tls stack mx_ipaddr emitter stream
              recipients
            >>= function
            | Ok () -> Lwt.return_ok ()
            | Error `STARTTLS_unavailable
            (* TODO(dinosaure): when [insecure]. *) ->
              Log.warn (fun m ->
                  m
                    "The SMTP receiver %a does not implement STARTTLS, restart \
                     in clear."
                    Domain.pp mx_domain);
              sendmail_without_tls ~encoder:(Fun.const encoder)
                ~decoder:(Fun.const decoder) ~info stack mx_ipaddr emitter
                stream recipients
            | Error err -> Lwt.return_error err)
        >>= function
        | Ok () -> Lwt.return_unit
        | Error err ->
          Log.err (fun m ->
              m "Impossible to send the given email to %a: %a." Domain.pp
                mx_domain pp_error err);
          go rest) in
    let sort =
      List.sort (fun {Ptt.Mxs.preference= a; _} {Ptt.Mxs.preference= b; _} ->
          icompare a b) in
    let mx_ipaddrs = Ptt.Mxs.elements mxs |> sort in
    go mx_ipaddrs

  let transmit ~pool ~info ~tls stack ?emitter (key, queue, consumer) resolved =
    let producers, targets =
      List.fold_left
        (fun (producers, targets) target ->
          let stream, producer = Lwt_stream.create () in
          let stream () = Lwt_scheduler.inj (Lwt_stream.get stream) in
          producer :: producers, (stream, target) :: targets)
        ([], []) resolved in
    let emitter = Option.value ~default:(fst (Ptt.Messaged.from key)) emitter in
    let transmit = plug_consumer_to_producers consumer producers in
    Log.debug (fun m ->
        m "Start to send the incoming email to %d recipient(s)."
          (List.length targets));
    Lwt.both (transmit ())
      (Lwt_list.iter_s
         (sendmail_to_a_target ~pool ~info ~tls ~key stack emitter)
         targets)
    >>= fun ((), ()) ->
    Log.debug (fun m -> m "Email sended!");
    Md.close queue
end
