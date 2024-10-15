open Rresult
open Lwt.Infix

let ( >>? ) = Lwt_result.bind

let src = Logs.Src.create "ptt.relay"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (Stack : Tcpip.Stack.V4V6) = struct
  include Ptt_flow.Make (Stack.TCP)

  type server =
    { info: info
    ; msgd: Msgd.t
    ; push: ((Msgd.key * string Lwt_stream.t * Msgd.result Lwt.u) option -> unit)
    ; mutable count: int64}
  
  and info = Ptt_common.info =
    { domain: Colombe.Domain.t
    ; ipaddr: Ipaddr.t
    ; tls: Tls.Config.server option
    ; zone: Mrmime.Date.Zone.t
    ; size: int64 }
  
  let info {info; _} = info

  let create ~info =
    let msgd, push = Lwt_stream.create () in
    let close () = push None in
    {info; msgd; push; count= 0L}, msgd, close
  
  let succ server =
    let v = server.count in
    server.count <- Int64.succ server.count;
    v
  
  type error = [ SMTP.error | Msgd.error | `Flow of string | `Invalid_recipients ]
  
  let pp_error ppf = function
    | #SMTP.error as err -> SMTP.pp_error ppf err
    | #Msgd.error as err -> Msgd.pp_error ppf err
    | `Flow msg -> Fmt.pf ppf "Error at the protocol level: %s" msg
    | `Invalid_recipients -> Fmt.string ppf "Invalid recipients"
  
  let properly_close_tls flow ctx =
    let encoder = Sendmail_with_starttls.Context_with_tls.encoder ctx in
    let m =
      SMTP.Value_with_tls.close encoder
      |> SMTP.Monad.reword_error (fun err -> `Tls err) in
    run flow m

  let dot = ".\r\n"

  let receive_mail ?(limit = 0x100000) flow ctx m push =
    let rec go count () =
      if count >= limit
      then begin push None; Lwt.return_error `Too_big_data end
      (* NOTE(dinosaure): [552] will be returned later. *)
      else
        run flow (m ctx) >>? function
          | ".." -> push (Some dot); go (count + 3) ()
        | "." -> push None; Lwt.return_ok ()
        | str ->
          let len = String.length str in
          let str = str ^ "\r\n" in
          push (Some str);
          go (count + len + 2) ()
    in
    go 0 ()

  let merge from_protocol from_logic =
    match from_protocol, from_logic with
    | Error `Too_big_data, _ -> `Too_big
    | Error `Not_enough_memory, _ -> `Not_enough_memory
    | Error `End_of_input, _ -> `Aborted
    | Error _, _ -> `Requested_action_not_taken `Temporary
    | Ok (), value -> value
  
  let accept :
         ?encoder:(unit -> bytes)
      -> ?decoder:(unit -> bytes)
      -> ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t)
      -> ipaddr:Ipaddr.t
      -> Stack.TCP.flow
      -> 'dns
      -> 'dns Ptt_common.resolver
      -> server
      -> (unit, error) result Lwt.t =
   fun ?encoder ?decoder ?queue ~ipaddr flow dns resolver server ->
    let ctx = Sendmail_with_starttls.Context_with_tls.make ?encoder ?decoder ?queue () in
    let m = SMTP.m_relay_init ctx server.info in
    let flow = make flow in
    run flow m >>? function
    | `Quit -> properly_close_tls flow ctx >>? fun () -> Lwt.return_ok ()
    | `Send {SMTP.domain_from; from; recipients; _} ->
      Ptt_common.recipients_are_reachable ~info:server.info dns resolver
        (List.map fst recipients)
      >>= function
      | true ->
        let id = succ server in
        let key = Msgd.key ~domain_from ~from ~recipients ~ipaddr id in
        let stream, push = Lwt_stream.create () in
        let th, wk = Lwt.task () in
        server.push (Some (key, stream, wk));
        let m = SMTP.m_mail ctx in
        run flow m >>? fun () ->
        receive_mail
          ~limit:(Int64.to_int server.info.size)
          flow ctx
          SMTP.(fun ctx -> Monad.recv ctx Value.Payload)
          push
        >>= fun result ->
        th >>= fun result' ->
        let m = SMTP.m_end (merge result result') ctx in
        run flow m >>? fun `Quit ->
        properly_close_tls flow ctx >>? fun () ->
        let result = match merge result result' with
          | `Ok -> Ok ()
          | #Msgd.error as err -> Error err in
        Lwt.return result
      | false ->
        let e = `Invalid_recipients in
        let m = SMTP.m_properly_close_and_fail ctx ~message:"No valid recipients" e in
        run flow m
end
