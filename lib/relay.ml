open Rresult
open Lwt.Infix

let ( >>? ) = Lwt_result.bind

let src = Logs.Src.create "ptt.relay"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (Stack : Tcpip.Stack.V4V6) = struct
  include Ptt_flow.Make (Stack.TCP)

  type server =
    { info: info
    ; messaged: Messaged.t
    ; push: ((Messaged.key * string Lwt_stream.t) option -> unit)
    ; mutable count: int64}
  
  and info = Ptt_common.info =
    { domain: Colombe.Domain.t
    ; ipaddr: Ipaddr.t
    ; tls: Tls.Config.server option
    ; zone: Mrmime.Date.Zone.t
    ; size: int64 }
  
  let info {info; _} = info

  let create ~info =
    let messaged, push = Lwt_stream.create () in
    let close () = push None in
    {info; messaged; push; count= 0L}, messaged, close
  
  let succ server =
    let v = server.count in
    server.count <- Int64.succ server.count;
    v
  
  type error = [ SMTP.error | `Too_big_data | `Flow of string | `Invalid_recipients ]
  
  let pp_error ppf = function
    | #SMTP.error as err -> SMTP.pp_error ppf err
    | `Too_big_data -> Fmt.pf ppf "Too big data"
    | `Flow msg -> Fmt.pf ppf "Error at the protocol level: %s" msg
    | `Invalid_recipients -> Fmt.string ppf "Invalid recipients"
  
  let properly_close_tls flow ctx =
    let encoder = Sendmail_with_starttls.Context_with_tls.encoder ctx in
    let m =
      SMTP.Value_with_tls.close encoder
      |> SMTP.Monad.reword_error (fun err -> `Tls err) in
    run flow m

  let dot = ".\r\n"

  let receive_mail ?(limit = 0x100000) flow ctx m bounded_stream =
    let rec go count () =
      if count >= limit then Lwt.return_error `Too_big_data
      else
        run flow (m ctx) >>? function
        | ".." -> bounded_stream#push dot >>= go (count + 3)
        | "." -> bounded_stream#close; Lwt.return_ok ()
        | str ->
          let len = String.length str in
          let str = str ^ "\r\n" in
          bounded_stream#push str >>=
          go (count + len + 2)
    in
    go 0 ()
  
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
        let key = Messaged.key ~domain_from ~from ~recipients ~ipaddr id in
        let stream, bounded_stream = Lwt_stream.create_bounded 0x7ff in
        server.push (Some (key, stream));
        let m = SMTP.m_mail ctx in
        run flow m >>? fun () ->
        receive_mail
          ~limit:(Int64.to_int server.info.size)
          flow ctx
          SMTP.(fun ctx -> Monad.recv ctx Value.Payload)
          bounded_stream
        >>? fun () ->
        let m = SMTP.m_end ctx in
        run flow m >>? fun `Quit ->
        properly_close_tls flow ctx >>? fun () -> Lwt.return_ok ()
      | false ->
        let e = `Invalid_recipients in
        let m = SMTP.m_properly_close_and_fail ctx ~message:"No valid recipients" e in
        run flow m
end
