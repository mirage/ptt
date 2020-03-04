module Lwt_backend = Lwt_backend
module type FLOW = Ptt.Sigs.FLOW with type +'a s = 'a Lwt.t

module Make (StackV4 : Mirage_stack.V4) = struct
  module TCP = Tuyau_mirage_tcp.Make(StackV4)

  open Rresult
  open Lwt.Infix
  open Lwt_backend

  let failwithf fmt = Fmt.kstrf (fun err -> Failure err) fmt

  let flow
    : type flow. (module Tuyau_mirage.FLOW with type flow = flow) -> (module FLOW with type t = flow)
    = fun (module Flow) ->
    let ic_raw = Cstruct.create 0x1000 in
    let oc_raw = Cstruct.create 0x1000 in

    let recv flow buf off len =
      let len = min len (Cstruct.len ic_raw) in
      let ic_raw = Cstruct.sub ic_raw 0 len in
      let rec fiber = function
        | Ok `End_of_input -> Lwt.return 0
        | Ok (`Input 0) -> Flow.recv flow ic_raw >>= fiber
        | Ok (`Input len) ->
          Cstruct.blit_to_bytes ic_raw 0 buf off len ;
          Lwt.return len
        | Error err -> Lwt.fail (failwithf "%a" Flow.pp_error err) in
      Flow.recv flow ic_raw >>= fiber in

    let rec send flow buf off len =
      let n = min len (Cstruct.len oc_raw) in
      Cstruct.blit_from_string buf off oc_raw 0 n ;
      Flow.send flow (Cstruct.sub oc_raw 0 n) >>= function
      | Ok n ->
        if n = len then Lwt.return () else send flow buf (off + n) (len - n)
      | Error err -> Lwt.fail (failwithf "%a" Flow.pp_error err) in
    let module Flow = struct type t = flow type +'a s = 'a Lwt.t let recv = recv let send = send end in
    (module Flow)

  let rdwr
    : type flow. (module Tuyau_mirage.FLOW with type flow = flow) -> (flow, Lwt_scheduler.t) Colombe.Sigs.rdwr
    = fun (module Flow) ->
    let ic_raw = Cstruct.create 0x1000 in
    let oc_raw = Cstruct.create 0x1000 in

    let recv flow buf off len =
      let len = min len (Cstruct.len ic_raw) in
      let ic_raw = Cstruct.sub ic_raw 0 len in
      let rec fiber = function
        | Ok `End_of_input -> Lwt.return 0
        | Ok (`Input 0) -> Flow.recv flow ic_raw >>= fiber
        | Ok (`Input len) ->
          Cstruct.blit_to_bytes ic_raw 0 buf off len ;
          Lwt.return len
        | Error err -> Lwt.fail (failwithf "%a" Flow.pp_error err) in
      Flow.recv flow ic_raw >>= fiber in

    let rec send flow buf off len =
      let n = min len (Cstruct.len oc_raw) in
      Cstruct.blit_from_string buf off oc_raw 0 n ;
      Flow.send flow (Cstruct.sub oc_raw 0 n) >>= function
      | Ok n ->
        if n = len then Lwt.return () else send flow buf (off + n) (len - n)
      | Error err -> Lwt.fail (failwithf "%a" Flow.pp_error err) in

    let rd flow buf off len = Lwt_scheduler.inj (recv flow buf off len) in
    let wr flow buf off len = Lwt_scheduler.inj (send flow buf off len) in

    { Colombe.Sigs.rd; Colombe.Sigs.wr; }

  module Flow
    : FLOW with type t = TCP.protocol
    = (val (flow (Tuyau_mirage.impl_of_flow TCP.protocol)))

  let sendmail ~info ?(tls= Tls.Config.client ~authenticator:X509.Authenticator.null ()) stack mx_ipaddr emitter producer recipients =
    let endpoint =
      { Tuyau_mirage_tcp.stack= stack
      ; Tuyau_mirage_tcp.keepalive= None
      ; Tuyau_mirage_tcp.nodelay= false
      ; Tuyau_mirage_tcp.ip= mx_ipaddr
      ; Tuyau_mirage_tcp.port= 25 } in
    Tuyau_mirage.flow_of_protocol ~key:TCP.endpoint endpoint ~protocol:TCP.protocol >>? fun flow ->
    let ctx = Sendmail_with_tls.Context_with_tls.make () in
    let rdwr = rdwr (Tuyau_mirage.impl_of_flow TCP.protocol) in
    let domain =
      let vs = Domain_name.to_strings info.Ptt.Logic.domain in
      Colombe.Domain.Domain vs in
    Lwt.catch
      (fun () ->
         Sendmail_with_tls.sendmail lwt rdwr flow ctx tls ~domain emitter recipients producer
         |> Lwt_scheduler.prj >|= R.reword_error (fun err -> `Sendmail err))
      (function Failure err -> Lwt.return (R.error_msg err) (* XXX(dinosaure): should come from [rdwr]. *)
              | exn -> Lwt.return (Error (`Exn exn))) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error (`Sendmail err) ->
      Lwt.return (R.error_msgf "%a" Sendmail_with_tls.pp_error err)
    | Error (`Msg _) as err ->
      Lwt.return err
    | Error (`Exn exn) ->
      Lwt.return (R.error_msgf "Unknown error: %s" (Printexc.to_string exn))
end
