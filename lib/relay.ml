open Colombe.Sigs
open Sigs

let ( <.> ) f g = fun x -> f (g  x)

module Make
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
= struct
  module Ke = Ke.Rke.Weighted
  module Md = Messaged.Make(Scheduler)(IO)

  let ( >>= ) = IO.bind

  type 'w resolver =
    { gethostbyname : 'a. 'w -> [ `host ] Domain_name.t -> (Ipaddr.V4.t, [> Rresult.R.msg ] as 'a) result IO.t
    ; extension : 'a. string -> string -> (Ipaddr.t, [> Rresult.R.msg ] as 'a) result IO.t }

  type 'w server =
    { info : info
    ; messaged : Md.t
    ; resolver : 'w resolver
    ; mutable count : int64 }
  and info = SMTP.info =
    { domain : Colombe.Domain.t
    ; ipv4 : Ipaddr.V4.t
    ; tls : Tls.Config.server
    ; size : int64 }

  let create ~info resolver =
    { info
    ; messaged= Md.create ()
    ; resolver
    ; count= 0L }

  let messaged { messaged; _ } = messaged

  let succ server =
    let v = server.count in
    server.count <- Int64.succ server.count ; v

  type error =
    [ `SMTP of SMTP.error
    | `Connection_close ]

  let pp_error ppf = function
    | `SMTP err -> SMTP.pp_error ppf err
    | `Connection_close -> Fmt.pf ppf "Connection close"

  let run
    : type flow. (flow, Scheduler.t) rdwr -> flow -> ('a, SMTP.error) Colombe.State.t -> ('a, error) result IO.t
    = fun { Colombe.Sigs.rd; wr; } flow m ->
      let rec go = function
        | Colombe.State.Read { buffer; off; len; k; } ->
          (rd flow buffer off len |> Scheduler.prj >>= function
            | 0 -> IO.return (Error `Connection_close)
            (* TODO(dinosaure): [rd] should returns [ `Close | `Len of int ]
               instead when [0] is specific to a file-descriptor. *)
            | n -> (go <.> k) n)
        | Colombe.State.Write { buffer; off; len; k; } ->
          wr flow buffer off len |> Scheduler.prj >>= fun () -> go (k len)
        | Colombe.State.Return v -> IO.return (Ok v)
        | Colombe.State.Error err -> IO.return (Error (`SMTP err)) in
      go m

  let ( >>- ) x f = x >>= function
    | Ok v -> f v
    | Error err -> IO.return (Error err)

  let resolve_recipients s w recipients =
    let open Colombe in
    let resolver = s.resolver in
    let rec go acc = function
      | [] -> IO.return (Ok (List.rev acc))
      | Forward_path.Postmaster :: r ->
        go (Ipaddr.V4 s.info.ipv4 :: acc) r
      | Forward_path.Forward_path { Path.domain= Domain.Domain v; _ } :: r
      | Forward_path.Domain (Domain.Domain v) :: r ->
        let domain =
          let open Rresult in
          Domain_name.of_strings v >>= Domain_name.host in
        IO.return domain
        >>- resolver.gethostbyname w
        >>- fun v -> go (Ipaddr.V4 v :: acc) r
      | Forward_path.Forward_path { Path.domain= Domain.IPv4 v; _ } :: r
      | Forward_path.Domain (Domain.IPv4 v) :: r ->
        go (Ipaddr.V4 v :: acc) r
      | Forward_path.Forward_path { Path.domain= Domain.IPv6 v; _ } :: r
      | Forward_path.Domain (Domain.IPv6 v) :: r ->
        go (Ipaddr.V6 v :: acc) r
      | Forward_path.Forward_path { Path.domain= Domain.Extension (ldh, v); _ } :: r
      | Forward_path.Domain (Domain.Extension (ldh, v)) :: r ->
        resolver.extension ldh v >>- fun v -> go (v :: acc) r in
    go [] recipients

  let receive_mail rdwr flow ctx producer =
    let dot = Some (".\r\n", 0, 3) in
    let m ctx = let open SMTP in Monad.recv ctx Value.Payload in
    (* TODO: limit! *)
    let rec go () =
      run rdwr flow (m ctx) >>- function
      | ".." -> producer dot >>= go
      | "." -> producer None >>= fun () -> IO.return (Ok ())
      | v -> producer (Some (v ^ "\r\n", 0, String.length v + 2)) >>= go in
    go ()

  let accept
    : type w flow. (flow, Scheduler.t) rdwr -> flow -> w -> w server -> (unit, error) result IO.t
    = fun rdwr flow resolver server ->
      let ctx = Sendmail_with_tls.Context_with_tls.make () in
      let m = SMTP.m_init ctx server.info in
      run rdwr flow m >>- function
      | `Quit -> IO.return (Ok ())
      | `Submission { SMTP.tls= false; _ } -> assert false (* TODO *)
      | `Submission { SMTP.domain_from; from; recipients; tls= true; _ } ->
        resolve_recipients server resolver (List.map fst recipients) >>= function
        | Ok targets ->
          let id = succ server in
          let recipients = List.map2 (fun (x, args) v -> (v, x, args)) recipients targets in
          let key = Messaged.v ~domain_from ~from ~recipients id in
          Md.push server.messaged key >>= fun producer ->
          let m = SMTP.m_mail ctx in
          run rdwr flow m >>- fun () ->
          receive_mail rdwr flow ctx producer >>- fun () ->
          let m = SMTP.m_end ctx in
          run rdwr flow m >>- fun `Quit ->
          IO.return (Ok ())
        | Error _ ->
          let e = `Protocol `Invalid_recipients in
          let m = SMTP.properly_close_and_fail ctx ~message:"No valid recipients" e in
          run rdwr flow m
end
