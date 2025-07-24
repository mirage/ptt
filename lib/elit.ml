open Rresult
open Lwt.Infix

let src = Logs.Src.create "ptt.elit"

module Log : Logs.LOG = (val Logs.src_log src)

let ( $ ) f g = fun x -> f (g x)
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

type 'k cfg = {
    limit: int
  ; info: Ptt_common.info
  ; locals: Ptt_map.t
  ; tls: Tls.Config.client
  ; random: Mirage_crypto_rng.g option
  ; hash: 'k Digestif.hash
  ; authentication: 'k Ptt.Authentication.t
  ; mechanisms: Ptt.Mechanism.t list
  ; submit_destination: Ipaddr.t
  ; submit_port: int option
  ; with_arc: bool
  ; mx_destination: Ipaddr.t option
  ; mx_port: int option
  ; admin: Colombe.Path.t
  ; sender: Colombe.Reverse_path.t
  ; allowed_to_forward: Ipaddr.Prefix.t list
  ; on_admin: sender:Colombe.Reverse_path.t -> string Lwt_stream.t -> unit Lwt.t
}

module Make
    (Stack : Tcpip.Stack.V4V6)
    (Dns_client : Dns_client_mirage.S)
    (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow) =
struct
  module Server = Ptt_server.Make (Stack)
  module Sendmail = Ptt_sendmail.Make (Stack) (Happy_eyeballs)
  (* module Uspf_client = Uspf_mirage.Make (Dns_client) *)

  module Local = struct
    module Submission = Ptt.Submission.Make (Stack)

    let submission_resolver =
      let open Ptt_common in
      let getmxbyname _ipaddrs mail_exchange =
        Dns.Rr_map.Mx_set.(singleton {Dns.Mx.preference= 0; mail_exchange})
        |> Lwt.return_ok in
      let gethostbyname ipaddrs _domain_name = Lwt.return_ok ipaddrs in
      {getmxbyname; gethostbyname}

    let submission_job ~pool ?stop cfg random stack server close =
      let handler flow =
        let ipaddr, port = Stack.TCP.dst flow in
        Lwt.finalize
          (fun () ->
            Lwt_pool.use pool @@ fun (encoder, decoder, _) ->
            Submission.accept_without_starttls ~encoder:(Fun.const encoder)
              ~decoder:(Fun.const decoder) ~ipaddr flow [cfg.submit_destination]
              submission_resolver random cfg.hash server
            >|= R.reword_error (R.msgf "%a" Submission.pp_error))
          (fun () -> Stack.TCP.close flow)
        >>= function
        | Ok () -> Lwt.return ()
        | Error (`Msg err) ->
          Log.err (fun m ->
              m "<%a:%d> raised an error: %s" Ipaddr.pp ipaddr port err);
          Lwt.return () in
      let port = Option.value ~default:465 cfg.submit_port in
      Server.init ~port stack >>= fun service ->
      Server.serve_when_ready ?stop ~handler service
      |> fun (`Initialized job) ->
      let job = job >|= close in
      job

    let submission_logic_job cfg (ic, oc) =
      let is_admin = function
        | [Colombe.Forward_path.Forward_path admin'] ->
          Colombe.Path.equal cfg.admin admin'
        | _ -> false in
      let on_exn exn =
        Log.err (fun m ->
            m "Got an error into the submission logic: %S"
              (Printexc.to_string exn));
        Lwt.return_unit in
      let fn key stream wk () =
        let info = cfg.info in
        let sender = fst (Ptt.Msgd.from key) in
        let recipients = Ptt.Msgd.recipients key in
        let recipients = List.map fst recipients in
        if is_admin recipients then
          cfg.on_admin ~sender stream >|= fun () -> Lwt.wakeup_later wk `Ok
        else begin
          let recipients = Ptt_map.expand ~info cfg.locals recipients in
          let recipients = Ptt_aggregate.to_recipients ~info recipients in
          let id = Ptt_common.id_to_messageID ~info (Ptt.Msgd.id key) in
          Log.debug (fun m ->
              m "%a submitted a new email %a." Colombe.Reverse_path.pp sender
                Mrmime.MessageID.pp id);
          let fn recipients =
            {
              Ptt_sendmail.sender
            ; recipients
            ; data= Lwt_stream.clone stream
            ; policies= []
            ; id
            } in
          let elts = List.map fn recipients in
          Log.debug (fun m ->
              m "Notice the SMTP server that everything is ok for %a from %a."
                Colombe.Reverse_path.pp sender Mrmime.MessageID.pp id);
          Lwt.wakeup_later wk `Ok;
          Log.debug (fun m ->
              m "Send the incoming email %a to our destination."
                Mrmime.MessageID.pp id);
          List.iter (oc $ Option.some) elts;
          Lwt.return_unit
        end in
      let rec go () =
        Lwt_stream.get ic >>= function
        | None -> oc None; Lwt.return_unit
        | Some (key, stream, wk) ->
          Lwt.catch (fn key stream wk) on_exn >>= Lwt.pause >>= go in
      go ()

    let job ?stop cfg stack he random =
      let pool0 =
        Lwt_pool.create cfg.limit @@ fun () ->
        let encoder = Bytes.create 0x7ff in
        let decoder = Bytes.create 0x7ff in
        let queue = Ke.Rke.create ~capacity:0x800 Bigarray.char in
        Lwt.return (encoder, decoder, queue) in
      let pool1 =
        Lwt_pool.create cfg.limit @@ fun () ->
        let encoder = Bytes.create 0x7ff in
        let decoder = Bytes.create 0x7ff in
        let queue = Ke.Rke.create ~capacity:0x800 Bigarray.char in
        Lwt.return (encoder, decoder, queue) in
      let pool1 = {Ptt_sendmail.pool= (fun fn -> Lwt_pool.use pool1 fn)} in
      let info = cfg.info
      and authenticator = cfg.authentication
      and resolver = submission_resolver
      and ms = cfg.mechanisms in
      let ic, stream0, close0 = Submission.create ~info ~authenticator ms in
      let oc, push0 = Sendmail.v ~resolver ~pool:pool1 ~info cfg.tls in
      Lwt.join
        [
          submission_job ~pool:pool0 ?stop cfg random stack ic close0
        ; submission_logic_job cfg (stream0, push0)
        ; Sendmail.job [cfg.submit_destination] he oc
        ]
  end

  module Out = struct
    module Relay = Ptt.Relay.Make (Stack)

    type resolver = Internet of Dns_client.t | Local of Ipaddr.t list

    let mail_exchange_resolver =
      let open Ptt_common in
      let getmxbyname resolver mail_exchange =
        match resolver with
        | Internet dns ->
          Dns_client.getaddrinfo dns Dns.Rr_map.Mx mail_exchange
          >|= Result.map snd
        | Local _ipaddrs ->
          Dns.Rr_map.Mx_set.(singleton {Dns.Mx.preference= 0; mail_exchange})
          |> Lwt.return_ok in
      let gethostbyname resolver domain_name =
        match resolver with
        | Local ipaddrs -> Lwt.return_ok ipaddrs
        | Internet dns -> (
          let ipv4 =
            Dns_client.gethostbyname dns domain_name
            >|= Result.map (fun ipv4 -> Ipaddr.V4 ipv4) in
          let ipv6 =
            Dns_client.gethostbyname6 dns domain_name
            >|= Result.map (fun ipv6 -> Ipaddr.V6 ipv6) in
          Lwt.all [ipv4; ipv6] >|= function
          | [Ok ipv4; Ok ipv6] -> Ok [ipv4; ipv6]
          | [Error _; Ok ipv6] -> Ok [ipv6]
          | [Ok ipv4; Error _] -> Ok [ipv4]
          | [(Error _ as err); _] -> err
          | [] | [_] | _ :: _ :: _ -> assert false) in
      {getmxbyname; gethostbyname}

    let mail_exchange_job ~pool ?stop cfg stack dns server close =
      let handler flow =
        let ipaddr, port = Stack.TCP.dst flow in
        Lwt.finalize
          (fun () ->
            Lwt_pool.use pool @@ fun (encoder, decoder, queue) ->
            Relay.accept ~encoder:(Fun.const encoder)
              ~decoder:(Fun.const decoder) ~queue:(Fun.const queue) ~ipaddr flow
              dns mail_exchange_resolver server
            >|= R.reword_error (R.msgf "%a" Relay.pp_error))
          (fun () -> Stack.TCP.close flow)
        >>= function
        | Ok () -> Lwt.return ()
        | Error (`Msg err) ->
          Log.err (fun m ->
              m "<%a:%d> raised an error: %s" Ipaddr.pp ipaddr port err);
          Lwt.return () in
      let port = Option.value ~default:25 cfg.mx_port in
      Server.init ~port stack >>= fun service ->
      Server.serve_when_ready ?stop ~handler service
      |> fun (`Initialized job) ->
      let job = job >|= close in
      job

    let forward_granted ipaddr allowed_to_forward =
      List.exists
        (fun prefix -> Ipaddr.Prefix.mem ipaddr prefix)
        allowed_to_forward

    let only_registered_recipients ~info map recipients =
      let for_all = function
        | Colombe.Forward_path.Postmaster -> true
        | Domain domain' -> Colombe.Domain.equal info.Ptt_common.domain domain'
        | Forward_path {Colombe.Path.local; domain= domain'; _} ->
          Colombe.Domain.equal info.Ptt_common.domain domain'
          && Ptt_map.exists ~local map in
      List.for_all for_all recipients

    let is_arc_seal =
      let open Mrmime.Field_name in
      equal (v "ARC-Seal")

    let get_unstrctrd_exn : type a. a Mrmime.Field.t -> a -> Unstrctrd.t =
     fun w v ->
      match w with
      | Mrmime.Field.Unstructured ->
        let fold acc = function
          | #Unstrctrd.elt as elt -> elt :: acc
          | _ -> acc in
        let unstrctrd = List.fold_left fold [] v in
        Result.get_ok (Unstrctrd.of_list (List.rev unstrctrd))
      | _ -> assert false

    let get_arc_signature :
           Unstrctrd.t
        -> (int * Dkim.signed Dkim.t * Dkim.map, [> `Msg of string ]) result =
     fun unstrctrd ->
      let ( let* ) = Result.bind in
      let* m = Dkim.of_unstrctrd_to_map unstrctrd in
      let none = msgf "Missing i field" in
      let* i = Option.to_result ~none (Dkim.get_key "i" m) in
      let* t = Dkim.map_to_t m in
      try
        let i = int_of_string i in
        Ok (i, t, m)
      with _exn -> error_msgf "Invalid Agent or User Identifier"

    let p =
      let open Mrmime in
      let unstructured = Field.(Witness Unstructured) in
      let open Field_name in
      Map.empty
      |> Map.add date unstructured
      |> Map.add from unstructured
      |> Map.add sender unstructured
      |> Map.add reply_to unstructured
      |> Map.add (v "To") unstructured
      |> Map.add cc unstructured
      |> Map.add bcc unstructured
      |> Map.add subject unstructured
      |> Map.add message_id unstructured
      |> Map.add comments unstructured
      |> Map.add content_type unstructured
      |> Map.add content_encoding unstructured

    let verify cfg ~sender ~ipaddr dns stream =
      Log.debug (fun m -> m "Verify the incoming email");
      let open Lwt.Syntax in
      let stream0 = Lwt_stream.clone stream in
      let stream1 = Lwt_stream.clone stream in
      let decoder = Mrmime.Hd.decoder p in
      let rec get_last_uid uid =
        let open Mrmime in
        match Hd.decode decoder with
        | `Field field ->
          let (Field.Field (fn, w, v)) = Location.prj field in
          if is_arc_seal fn then
            match get_arc_signature (get_unstrctrd_exn w v) with
            | Ok (uid', _, _) -> get_last_uid (Int.max uid uid')
            | Error _ -> get_last_uid uid
          else get_last_uid uid
        | `Malformed _ | `End _ ->
          Lwt.return uid
          (* XXX(dinosaure): if it fails, the second loop [go] will fail also. *)
        | `Await -> (
          Lwt_stream.get stream1 >>= function
          | Some str ->
            Hd.src decoder str 0 (String.length str);
            get_last_uid uid
          | None ->
            Hd.src decoder String.empty 0 0;
            get_last_uid uid) in
      let rec go decoder =
        match Dmarc.Verify.decode decoder with
        | #Dmarc.Verify.error as err -> Lwt.return_error err
        | `Info value -> Lwt.return_ok value
        | `Query (decoder, domain_name, Dns.Rr_map.K record) ->
          let* response =
            Dns_client.get_resource_record dns record domain_name in
          go (Dmarc.Verify.response decoder record response)
        | `Await decoder -> (
          Lwt_stream.get stream0 >>= function
          | Some str -> go (Dmarc.Verify.src decoder str 0 (String.length str))
          | None -> go (Dmarc.Verify.src decoder String.empty 0 0)) in
      let ctx =
        Uspf.empty |> Uspf.with_ip ipaddr |> fun ctx ->
        let some = fun v -> Uspf.with_sender (`MAILFROM v) ctx in
        Option.fold ~none:ctx ~some sender in
      let dkims_are_valid dkims =
        let fn = function Dmarc.DKIM.Pass _ -> true | _ -> false in
        List.for_all fn dkims in
      Lwt.both (get_last_uid 0) (go (Dmarc.Verify.decoder ~ctx ())) >>= function
      | uid, Ok ((_, dkims, _) as results) when dkims_are_valid dkims ->
        let receiver =
          match cfg.info.Ptt_common.domain with
          | Colombe.Domain.Domain ds -> `Domain ds
          | IPv4 ipv4 -> `Addr (Emile.IPv4 ipv4)
          | IPv6 ipv6 -> `Addr (Emile.IPv6 ipv6)
          | Extension (k, v) -> `Addr (Emile.Ext (k, v)) in
        let encoder =
          match cfg.with_arc with
          | true -> Arc.Encoder.stamp_results ~receiver ~uid:(succ uid)
          | false ->
            let encoder ppf v =
              let open Prettym in
              eval ppf
                [
                  string $ "Authentication-Results"; char $ ':'; spaces 1
                ; !!(Dmarc.Encoder.field ~receiver)
                ]
                v in
            encoder in
        let authentication_results = Prettym.to_string encoder results in
        let prefix = Lwt_stream.of_list [authentication_results] in
        Lwt.return (`Ok (Lwt_stream.append prefix stream))
      | _, Ok _ ->
        Log.warn (fun m -> m "DKIM-Signatures of the incoming email failed");
        Lwt.return `Aborted
      | _, Error err ->
        Log.warn (fun m -> m "DMAR error: %a" Dmarc.Verify.pp_error err);
        Lwt.return `Aborted

    let mail_exchange_logic_job cfg dns (ic, oc_local, oc_inter) =
      let info = cfg.info in
      let rec go () =
        Lwt_stream.get ic >>= function
        | None -> oc_local None; oc_inter None; Lwt.return_unit
        | Some (key, stream, wk) -> begin
          let id = Ptt_common.id_to_messageID ~info (Ptt.Msgd.id key) in
          Log.debug (fun m ->
              m "%a sent a new email %a to: @[<hov>%a@]."
                Colombe.Reverse_path.pp cfg.sender Mrmime.MessageID.pp id
                Fmt.(Dump.list Colombe.Forward_path.pp)
                (List.map fst (Ptt.Msgd.recipients key)));
          let fake_recipients = Ptt.Msgd.recipients key in
          let fake_recipients = List.map fst fake_recipients in
          let real_recipients =
            Ptt_map.expand ~info cfg.locals fake_recipients in
          Log.debug (fun m ->
              m "real recipients of %a: @[<hov>%a@]" Mrmime.MessageID.pp id
                Fmt.(Dump.list Colombe.Forward_path.pp)
                real_recipients);
          let real_recipients =
            Ptt_aggregate.to_recipients ~info real_recipients in
          Log.debug (fun m ->
              m "forward email granted? %b"
                (forward_granted (Ptt.Msgd.ipaddr key) cfg.allowed_to_forward));
          begin
            if forward_granted (Ptt.Msgd.ipaddr key) cfg.allowed_to_forward then
              Lwt.return (`Ok stream)
            else
              verify cfg
                ~sender:(fst (Ptt.Msgd.from key))
                ~ipaddr:(Ptt.Msgd.ipaddr key) dns stream
          end
          >>= function
          | #Ptt.Msgd.error as err ->
            Log.warn (fun m ->
                m
                  "Can not verify {,DM}ARC informations from %a for %a, \
                   discard it!"
                  Colombe.Reverse_path.pp
                  (fst (Ptt.Msgd.from key))
                  Mrmime.MessageID.pp id);
            Log.warn (fun m -> m "%a" Ptt.Msgd.pp_error err);
            Lwt.wakeup_later wk err;
            Lwt.pause () >>= go
          | `Ok stream ->
            let fn recipients =
              {
                Ptt_sendmail.sender= cfg.sender
              ; recipients
              ; data= Lwt_stream.clone stream
              ; policies= []
              ; id
              } in
            let elts = List.map fn real_recipients in
            let src = Ptt.Msgd.ipaddr key in
            if
              forward_granted src cfg.allowed_to_forward
              || only_registered_recipients ~info cfg.locals fake_recipients
            then begin
              let to_internet =
                forward_granted src cfg.allowed_to_forward
                && not
                     (only_registered_recipients ~info cfg.locals
                        fake_recipients) in
              let oc = if to_internet then oc_inter else oc_local in
              Log.debug (fun m ->
                  m "Send the incoming email to Internet? %b" to_internet);
              List.iter (oc $ Option.some) elts;
              Log.debug (fun m ->
                  m
                    "Notice the SMTP server that everything is ok for %a from \
                     %a (%a)."
                    Mrmime.MessageID.pp id Colombe.Reverse_path.pp
                    (fst (Ptt.Msgd.from key))
                    Ipaddr.pp (Ptt.Msgd.ipaddr key));
              Lwt.wakeup_later wk `Ok
            end
            else begin
              Log.warn (fun m ->
                  m "Email %a to unknown users (%a), discard it!"
                    Mrmime.MessageID.pp id
                    Fmt.(Dump.list Colombe.Forward_path.pp)
                    fake_recipients);
              Lwt.wakeup_later wk (`Requested_action_not_taken `Permanent)
            end;
            Lwt.pause () >>= go
        end in
      go ()

    let job ?(limit = 20) ?stop cfg stack dns he =
      let resolver = mail_exchange_resolver in
      let info = cfg.info in
      let push_local, push_inter, job =
        match cfg.mx_destination with
        | None ->
          let pool0 =
            Lwt_pool.create limit @@ fun () ->
            let encoder = Bytes.create 0x7ff in
            let decoder = Bytes.create 0x7ff in
            let queue = Ke.Rke.create ~capacity:0x800 Bigarray.char in
            Lwt.return (encoder, decoder, queue) in
          let pool0 = {Ptt_sendmail.pool= (fun fn -> Lwt_pool.use pool0 fn)} in
          let oc, push0 = Sendmail.v ~resolver ~pool:pool0 ~info cfg.tls in
          push0, push0, Sendmail.job (Internet dns) he oc
        | Some ipaddr ->
          let pool0 =
            Lwt_pool.create limit @@ fun () ->
            let encoder = Bytes.create 0x7ff in
            let decoder = Bytes.create 0x7ff in
            let queue = Ke.Rke.create ~capacity:0x800 Bigarray.char in
            Lwt.return (encoder, decoder, queue) in
          let pool0 = {Ptt_sendmail.pool= (fun fn -> Lwt_pool.use pool0 fn)} in
          let pool1 =
            Lwt_pool.create limit @@ fun () ->
            let encoder = Bytes.create 0x7ff in
            let decoder = Bytes.create 0x7ff in
            let queue = Ke.Rke.create ~capacity:0x800 Bigarray.char in
            Lwt.return (encoder, decoder, queue) in
          let pool1 = {Ptt_sendmail.pool= (fun fn -> Lwt_pool.use pool1 fn)} in
          let local, push0 = Sendmail.v ~resolver ~pool:pool0 ~info cfg.tls in
          let inter, push1 = Sendmail.v ~resolver ~pool:pool1 ~info cfg.tls in
          let job =
            Lwt.join
              [
                Sendmail.job (Local [ipaddr]) he local
              ; Sendmail.job (Internet dns) he inter
              ] in
          push0, push1, job in
      let pool0 =
        Lwt_pool.create limit @@ fun () ->
        let encoder = Bytes.create 0x7ff in
        let decoder = Bytes.create 0x7ff in
        let queue = Ke.Rke.create ~capacity:0x800 Bigarray.char in
        Lwt.return (encoder, decoder, queue) in
      let ic, stream0, close0 = Relay.create ~info in
      let dns : Dns_client.t = dns in
      Lwt.join
        [
          mail_exchange_job ~pool:pool0 ?stop cfg stack (Internet dns) ic close0
        ; mail_exchange_logic_job cfg dns (stream0, push_local, push_inter); job
        ]
  end

  type 'k iter =
       (Ptt_map.local -> 'k Digestif.t -> Emile.mailbox list -> unit Lwt.t)
    -> unit Lwt.t

  let ignore_admin ~sender:_ _ = Lwt.return_unit

  let v
      ~info
      ?g
      ?(mechanisms = [Ptt.Mechanism.PLAIN])
      ~postmaster
      ?forward_granted:(allowed_to_forward = [])
      ?with_arc
      ?mx_destination
      ?(on_admin = ignore_admin)
      hash
      iter
      submit_destination =
    let admin =
      let local = `Dot_string ["admin"] in
      let domain = info.Ptt_common.domain in
      Colombe.Path.{local; domain; rest= []} in
    let sender =
      let local = `Dot_string ["ptt"; "elit"] in
      let domain = info.Ptt_common.domain in
      Some Colombe.Path.{local; domain; rest= []} in
    let with_arc =
      match with_arc, mx_destination with
      | None, None -> false
      | None, Some _ -> true
      | Some value, _ -> value in
    let authenticator = R.failwith_error_msg (Ca_certs_nss.authenticator ()) in
    let tls =
      Rresult.R.failwith_error_msg (Tls.Config.client ~authenticator ()) in
    let locals = Ptt_map.empty ~postmaster in
    let passwds = Hashtbl.create 0x100 in
    let add local passwd dsts =
      List.iter (fun dst -> Ptt_map.add ~local dst locals) dsts;
      Hashtbl.add passwds local passwd;
      Lwt.return_unit in
    iter add >|= fun () ->
    let authentication local passwd' =
      match Hashtbl.find_opt passwds local with
      | Some passwd -> Lwt.return (Digestif.equal hash passwd passwd')
      | None -> Lwt.return false in
    let authentication = Ptt.Authentication.v authentication in
    {
      limit= 20
    ; info
    ; locals
    ; tls
    ; random= g
    ; hash
    ; authentication
    ; mechanisms
    ; submit_destination
    ; submit_port= Some 465
    ; mx_destination
    ; mx_port= Some 25
    ; allowed_to_forward
    ; with_arc
    ; admin
    ; sender
    ; on_admin
    }

  let job ?stop cfg ?submission ?relay stack dns he =
    let submission = {cfg.info with Ptt_common.tls= submission} in
    let relay = {cfg.info with Ptt_common.tls= relay} in
    Lwt.join
      [
        Local.job ?stop {cfg with info= submission} stack he cfg.random
      ; Out.job ?stop {cfg with info= relay} stack dns he
      ]
end
