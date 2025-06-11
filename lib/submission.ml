open Rresult
open Lwt.Infix

let ( >>? ) = Lwt_result.bind
let src = Logs.Src.create "ptt.submission"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (Stack : Tcpip.Stack.V4V6) = struct
  module Tls_flow = Tls_mirage.Make (Stack.TCP)
  module TLS = Ptt_flow.Make (Tls_flow)
  module TCP = Ptt_flow.Make (Stack.TCP)

  type 'k server = {
      info: info
    ; msgd: Msgd.t
    ; push: (Msgd.key * string Lwt_stream.t * Msgd.result Lwt.u) option -> unit
    ; mechanisms: Mechanism.t list
    ; authenticator: 'k Authentication.t
    ; mutable count: int64
  }

  and info = SSMTP.info = {
      domain: Colombe.Domain.t
    ; ipaddr: Ipaddr.t
    ; tls: Tls.Config.server option
    ; zone: Mrmime.Date.Zone.t
    ; size: int64
  }

  let info {info; _} = info

  let create ~info ~authenticator mechanisms =
    let msgd, push = Lwt_stream.create () in
    let close () = push None in
    {info; msgd; push; mechanisms; authenticator; count= 0L}, msgd, close

  let succ server =
    let v = server.count in
    server.count <- Int64.succ server.count;
    v

  type error =
    [ SSMTP.error
    | Msgd.error
    | `Too_many_tries
    | `Flow of string
    | `Invalid_recipients
    | `End_of_input ]

  type 'err runner =
    | Runner : {
          run:
            'a. 'flow -> ('a, 'err) Colombe.State.t -> ('a, 'err) result Lwt.t
        ; flow: 'flow
      }
        -> 'err runner

  let flowf fmt = Fmt.kstr (fun str -> `Flow str) fmt

  let pp_error ppf = function
    | #SSMTP.error as err -> SSMTP.pp_error ppf err
    | #Msgd.error as err -> Msgd.pp_error ppf err
    | `Too_many_tries -> Fmt.pf ppf "Too many tries"
    | `Flow msg -> Fmt.pf ppf "Error at the protocol level: %s" msg
    | `Invalid_recipients -> Fmt.string ppf "Invalid recipients"
    | `End_of_input -> Fmt.string ppf "End of input"

  let authentication
      ctx
      ~domain_from
      (Runner {run; flow})
      random
      hash
      server
      ?payload
      mechanism =
    let rec go limit ?payload m =
      if limit >= 3 then
        let e = `Too_many_tries in
        let m =
          SSMTP.m_properly_close_and_fail ctx ~message:"Too many tries" e in
        run flow m
      else
        match m, payload with
        | Mechanism.PLAIN, Some v -> begin
          Authentication.decode_authentication hash (Authentication.PLAIN None)
            server.authenticator v
          >>= function
          | Ok (user, true) ->
            let m = SSMTP.(Monad.send ctx Value.PP_235 ["Accepted, buddy!"]) in
            run flow m >>? fun () -> Lwt.return_ok (`Authenticated user)
          | (Error _ | Ok (_, false)) as res -> begin
            let () =
              match res with
              | Error (`Msg err) ->
                Log.err (fun m -> m "Got an authentication error: %s" err)
              | _ -> () in
            let m =
              let open SSMTP in
              let open Monad in
              let* () = send ctx Value.PN_535 ["Bad authentication, buddy!"] in
              SSMTP.m_submission ctx ~domain_from server.mechanisms in
            run flow m >>? function
            | `Quit -> Lwt.return_ok `Quit
            | `Authentication (_domain_from, m) ->
              (* assert (_domain_from = domain_from) ; *)
              go (limit + 1) m
            | `Authentication_with_payload (_domain_from, m, payload) ->
              (* assert (_domain_from = domain_from) ; *)
              go (limit + 1) ~payload m
          end
        end
        | Mechanism.PLAIN, None -> begin
          let stamp = Bytes.create 0x10 in
          Mirage_crypto_rng.generate_into ?g:random stamp 0x10;
          let stamp = Bytes.unsafe_to_string stamp in
          Log.debug (fun m -> m "Generate the stamp %S." stamp);
          let m =
            let open SSMTP in
            let open Monad in
            send ctx Value.TP_334 [Base64.encode_string ~pad:true stamp]
            >>= fun () -> recv ctx Value.Payload in
          run flow m >>? fun v ->
          Log.debug (fun m -> m "Got a payload while authentication: %S" v);
          Authentication.decode_authentication hash
            (Authentication.PLAIN (Some stamp)) server.authenticator v
          >>= function
          | Ok (user, true) ->
            let m = SSMTP.(Monad.send ctx Value.PP_235 ["Accepted, buddy!"]) in
            run flow m >>? fun () -> Lwt.return_ok (`Authenticated user)
          | (Error _ | Ok (_, false)) as res -> (
            let () =
              match res with
              | Error (`Msg err) ->
                Log.err (fun m -> m "Got an authentication error: %s" err)
              | _ -> () in
            let m =
              let open SSMTP in
              let open Monad in
              let* () = send ctx Value.PN_535 ["Bad authentication, buddy!"] in
              SSMTP.m_submission ctx ~domain_from server.mechanisms in
            run flow m >>? function
            | `Quit -> Lwt.return_ok `Quit
            | `Authentication (_domain_from, m) ->
              (* assert (_domain_from = domain_from) ; *)
              go (limit + 1) m
            | `Authentication_with_payload (_domain_from, m, payload) ->
              (* assert (_domain_from = domain_from) ; *)
              go (limit + 1) ~payload m)
        end in
    go 1 ?payload mechanism

  type authentication =
    [ `Authentication_with_payload of Colombe.Domain.t * Mechanism.t * string
    | `Authentication of Colombe.Domain.t * Mechanism.t ]

  let dot = ".\r\n"

  let receive_mail ?(limit = 0x100000) (Runner {run; flow}) ctx m push =
    let rec go count () =
      if count >= limit then begin
        push None; Lwt.return_error `Too_big
      end
      else
        run flow (m ctx) >>? function
        | ".." ->
          push (Some dot);
          go (count + 3) ()
        | "." -> push None; Lwt.return_ok ()
        | str ->
          let len = String.length str in
          let str = str ^ "\r\n" in
          push (Some str);
          go (count + len + 2) () in
    go 0 ()

  let merge from_protocol from_logic =
    match from_protocol, from_logic with
    | Error `Too_big, _ -> `Too_big
    | Error `Not_enough_memory, _ -> `Not_enough_memory
    | Error `End_of_input, _ -> `Aborted
    | Error _, _ -> `Requested_action_not_taken `Temporary
    | Ok (), value -> value

  let accept_without_starttls :
         ?encoder:(unit -> bytes)
      -> ?decoder:(unit -> bytes)
      -> ipaddr:Ipaddr.t
      -> Stack.TCP.flow
      -> 'dns
      -> 'dns Ptt_common.resolver
      -> Mirage_crypto_rng.g option
      -> 'k Digestif.hash
      -> 'k server
      -> (unit, error) result Lwt.t =
   fun ?encoder ?decoder ~ipaddr flow dns resolver random hash server ->
    let ctx = Colombe.State.Context.make ?encoder ?decoder () in
    let m = SSMTP.m_submission_init ctx server.info server.mechanisms in
    begin
      match server.info.SSMTP.tls with
      | None -> Lwt.return_ok (Runner {run= TCP.run; flow= TCP.make flow})
      | Some tls ->
        Tls_flow.server_of_flow tls flow
        >|= Result.map_error (flowf "%a" Tls_flow.pp_write_error)
        >>? fun flow ->
        Lwt.return_ok (Runner {run= TLS.run; flow= TLS.make flow})
    end
    >>? fun (Runner {run; flow} as runner) ->
    run flow m >>? function
    | `Quit -> Lwt.return_ok ()
    | #authentication as auth -> (
      let domain_from, m, payload =
        match auth with
        | `Authentication_with_payload (domain_from, m, v) ->
          domain_from, m, Some v
        | `Authentication (domain_from, m) -> domain_from, m, None in
      authentication ctx ~domain_from runner random hash server ?payload m
      >>? function
      | `Quit -> Lwt.return_ok ()
      | `Authenticated user -> (
        let m = SSMTP.m_relay ctx ~domain_from in
        run flow m >>? function
        | `Quit -> Lwt.return_ok ()
        | `Send {SSMTP.domain_from; recipients; from; _} -> (
          Ptt_common.recipients_are_reachable ~info:server.info dns resolver
            (List.map fst recipients)
          >>= function
          | true -> begin
            let id = succ server in
            let from =
              let sender =
                Colombe.Path.
                  {local= user; domain= server.info.SSMTP.domain; rest= []}
              in
              Some sender, snd from in
            let key = Msgd.key ~domain_from ~from ~recipients ~ipaddr id in
            let stream, push = Lwt_stream.create () in
            let th, wk = Lwt.task () in
            server.push (Some (key, stream, wk));
            let m = SSMTP.m_mail ctx in
            run flow m >>? fun () ->
            receive_mail
              ~limit:(Int64.to_int server.info.size)
              runner ctx
              SSMTP.(fun ctx -> Monad.recv ctx Value.Payload)
              push
            >>= fun result ->
            Log.debug (fun m ->
                m "Email received, waiting result from the logic");
            th >>= fun result' ->
            let m = SSMTP.m_end (merge result result') ctx in
            run flow m >>? fun `Quit ->
            let result =
              match merge result result' with
              | `Ok -> Ok ()
              | #Msgd.error as err -> Error err in
            Lwt.return result
          end
          | false ->
            let e = `Invalid_recipients in
            let m =
              SSMTP.m_properly_close_and_fail ctx ~message:"No valid recipients"
                e in
            run flow m)))
end
