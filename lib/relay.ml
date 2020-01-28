open Colombe.Sigs
open Sigs
open Rresult

let ( <.> ) f g = fun x -> f (g  x)

module Make
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
= struct
  module Ke = Ke.Rke.Weighted
  module Md = Messaged.Make(Scheduler)(IO)

  let ( >>= ) = IO.bind

  let src = Logs.Src.create "ptt-relay"
  module Log = (val Logs.src_log src : Logs.LOG)

  type 'w resolver =
    { gethostbyname : 'a. 'w -> [ `host ] Domain_name.t -> (Ipaddr.V4.t, [> R.msg ] as 'a) result IO.t
    ; getmxbyname : 'a. 'w -> [ `host ] Domain_name.t -> (Dns.Rr_map.Mx_set.t, [> R.msg ] as 'a) result IO.t
    ; extension : 'a. string -> string -> (Ipaddr.V4.t, [> R.msg ] as 'a) result IO.t }

  type 'w server =
    { info : info
    ; messaged : Md.t
    ; resolver : 'w resolver
    ; mutable count : int64 }
  and info = SMTP.info =
    { domain : [ `host ] Domain_name.t
    ; ipv4 : Ipaddr.V4.t
    ; tls : Tls.Config.server
    ; size : int64 }

  let info { info; _ } = info
  let resolver { resolver; _ } = resolver

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

  let io_fold_list ~f a l =
    let rec go a = function
      | [] -> IO.return a
      | x :: r -> f x a >>= fun a -> go a r in
    go a l

  (* XXX(dinosaure): this function checks only if domains have a reachable Mail Exchange service. *)
  let recipients_are_reachable s w recipients =
    let open Colombe in
    let fold { Dns.Mx.mail_exchange; Dns.Mx.preference; } m =
      s.resolver.gethostbyname w mail_exchange >>= function
      | Ok mx_ipaddr ->
        let mx_ipaddr = Ipaddr.V4 mx_ipaddr in (* TODO: [gethostbyname] should return a [Ipaddr.t]. *)
        IO.return (Mxs.add { Mxs.preference; Mxs.mx_ipaddr; Mxs.mx_domain= Some mail_exchange; } m)
      | Error (`Msg err) ->
        Log.warn (fun m -> m "Impossible to reach the Mail Exchange service %a: %s" Domain_name.pp mail_exchange err) ;
        IO.return m in
    let rec go acc = function
      | [] -> IO.return acc
      | Forward_path.Postmaster :: r ->
        go (Mxs.(singleton (v ~preference:0 (Ipaddr.V4 s.info.ipv4))) :: acc) r
      | Forward_path.Forward_path { Path.domain= Domain.Domain v; _ } :: r
      | Forward_path.Domain (Domain.Domain v) :: r ->
        ( try
            let domain = Domain_name.(host_exn <.> of_strings_exn) v in
            s.resolver.getmxbyname w domain
            >>= function
            | Ok m ->
              io_fold_list ~f:fold Mxs.empty (Dns.Rr_map.Mx_set.elements m) >>= fun s ->
              go (s :: acc) r
            | Error (`Msg err) ->
              Log.warn (fun m -> m "Got an error while resolving %a: %s" Domain_name.pp domain err) ;
              go acc r
          with _exn ->
            Log.err (fun m -> m "%a is an invalid-domain." Domain.pp (Domain.Domain v)) ;
            go (Mxs.empty :: acc) r )
      | Forward_path.Forward_path { Path.domain= Domain.IPv4 mx_ipaddr; _ } :: r
      | Forward_path.Domain (Domain.IPv4 mx_ipaddr) :: r ->
        go (Mxs.(singleton (v ~preference:0 (Ipaddr.V4 mx_ipaddr))) :: acc) r
      | Forward_path.Forward_path { Path.domain= Domain.IPv6 v; _ } :: r
      | Forward_path.Domain (Domain.IPv6 v) :: r ->
        Log.err (fun m -> m "Impossible to resolve an IPv6 domain: %a" Ipaddr.V6.pp v) ;
        go acc r
      | Forward_path.Forward_path { Path.domain= Domain.Extension (ldh, v); _ } :: r
      | Forward_path.Domain (Domain.Extension (ldh, v)) :: r ->
        s.resolver.extension ldh v >>= function
        | Ok mx_ipaddr -> go (Mxs.(singleton (v ~preference:0 (Ipaddr.V4 mx_ipaddr))) :: acc) r
        | Error (`Msg err) ->
          Log.warn (fun m -> m "Got an error while resolving [%s:%s]: %s" ldh v err) ;
          go acc r in
    go [] recipients >>= (IO.return <.> List.for_all (fun m -> not (Mxs.is_empty m)))

  let dot = Some (".\r\n", 0, 3)

  let receive_mail rdwr flow ctx producer =
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
        recipients_are_reachable server resolver (List.map fst recipients) >>= function
        | true ->
          let id = succ server in
          let key = Messaged.v ~domain_from ~from ~recipients id in
          Md.push server.messaged key >>= fun producer ->
          let m = SMTP.m_mail ctx in
          run rdwr flow m >>- fun () ->
          receive_mail rdwr flow ctx producer >>- fun () ->
          let m = SMTP.m_end ctx in
          run rdwr flow m >>- fun `Quit ->
          IO.return (Ok ())
        | false ->
          let e = `Protocol `Invalid_recipients in
          let m = SMTP.m_properly_close_and_fail ctx ~message:"No valid recipients" e in
          run rdwr flow m
end
