let ( <.> ) f g x = f (g x)

let apply x f = f x

let icompare : int -> int -> int = fun a b -> compare a b

module Make
    (Pclock : Mirage_clock.PCLOCK)
    (StackV4 : Mirage_stack.V4)
    (Md : Ptt.Messaged.S with type 'a s = 'a Lwt.t) =
struct
  open Lwt.Infix
  open Ptt_tuyau.Lwt_backend
  include Ptt_tuyau.Make (StackV4)

  let local_to_forward_path ~domain:mx_domain local =
    let local =
      `Dot_string (List.map (function `Atom x -> x | `String x -> x) local)
    in
    Colombe.Forward_path.Forward_path
      {
        Colombe.Path.local;
        Colombe.Path.domain = mx_domain;
        Colombe.Path.rest = [];
      }

  let plug_consumer_to_producers consumer producers =
    let rec go () =
      consumer () >>= function
      | Some v ->
          List.iter (fun producer -> producer (Some v)) producers ;
          Lwt.pause () >>= go
      | None ->
          List.iter (fun producer -> producer None) producers ;
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
            if !current == s1
            then Lwt.return None
            else (
              current := s1 ;
              Lwt_scheduler.prj (next ())) in
      Lwt_scheduler.inj res in
    next

  let transmit ~info stack (key, queue, consumer) resolved =
    let producers, targets =
      List.fold_left
        (fun (producers, targets) target ->
          let stream, producer = Lwt_stream.create () in
          let stream () = Lwt_scheduler.inj (Lwt_stream.get stream) in
          (producer :: producers, (stream, target) :: targets))
        ([], []) resolved in
    let emitter, _ = Ptt.Messaged.from key and id = Ptt.Messaged.id key in
    let received recipient =
      let by = Domain_name.to_strings info.Ptt.Logic.domain in
      let id =
        let open Mrmime.Mailbox in
        (Local.(v [ w (Fmt.strf "%08LX" id) ]), `Domain by) in
      let received =
        Received.make
          ~from:(Received.Only (Ptt.Messaged.domain_from key))
          ~by:(Received.Only (Colombe.Domain.Domain by)) ~via:Received.tcp
          ~protocol:Received.esmtp ~id ~zone:info.Ptt.Logic.zone recipient
          (Ptime.v (Pclock.now_d_ps ())) in
      let stream = Prettym.to_stream Received.Encoder.as_field received in
      let stream =
        Lwt_stream.from (Lwt.return <.> stream)
        |> Lwt_stream.map (fun s -> (s, 0, String.length s)) in
      Lwt_scheduler.inj <.> fun () -> Lwt_stream.get stream in
    let transmit = plug_consumer_to_producers consumer producers in
    let sendmails =
      let sendmail (stream, (k, vs)) () =
        let open Colombe in
        let open Forward_path in
        let mx_domain, mxs =
          match k with
          | `Ipaddr (Ipaddr.V4 v4 as mx_ipaddr) ->
              (Domain.IPv4 v4, Ptt.Mxs.(singleton (v ~preference:0 mx_ipaddr)))
          | `Ipaddr (Ipaddr.V6 v6 as mx_ipaddr) ->
              (Domain.IPv6 v6, Ptt.Mxs.(singleton (v ~preference:0 mx_ipaddr)))
          | `Domain (mx_domain, mxs) ->
              (Domain.Domain (Domain_name.to_strings mx_domain), mxs) in
        let recipients =
          match vs with
          | `All -> [ Domain mx_domain ]
          | `Local vs -> List.map (local_to_forward_path ~domain:mx_domain) vs
        in
        let received =
          match recipients with
          | [ Forward_path path ] -> received (Some path)
          | _ -> received None in
        let stream = received <+> stream in
        let rec go = function
          | [] -> Lwt.return ()
          | { Ptt.Mxs.mx_ipaddr = Ipaddr.V6 _; _ } :: rest -> go rest
          | { Ptt.Mxs.mx_ipaddr = Ipaddr.V4 mx_ipaddr; _ } :: rest -> (
              sendmail ~info stack mx_ipaddr emitter stream recipients
              >>= function
              | Ok () -> Lwt.return ()
              | Error _ -> go rest) in
        let sort =
          List.sort
            (fun { Ptt.Mxs.preference = a; _ } { Ptt.Mxs.preference = b; _ } ->
              icompare a b) in
        let sorted = Ptt.Mxs.elements mxs |> sort in
        go sorted in
      List.map sendmail targets in
    Lwt.join (List.map (apply ()) (transmit :: sendmails)) >>= fun () ->
    Md.close queue
end
