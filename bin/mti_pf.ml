let () = Printexc.record_backtrace true
let reporter = Logs_fmt.reporter ()
let () = Fmt.set_utf_8 Fmt.stdout true
let () = Fmt.set_utf_8 Fmt.stderr true
let () = Fmt.set_style_renderer Fmt.stdout `Ansi_tty
let () = Fmt.set_style_renderer Fmt.stderr `Ansi_tty
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Logs.set_reporter reporter

let ( <.> ) f g = fun x -> f (g x)

open Lwt_backend

module Relay = Ptt.Relay.Make(Lwt_scheduler)(Lwt_io)

let resolver =
  { Relay.gethostbyname= (fun w v -> Dns_client_lwt.gethostbyname w v)
  ; Relay.extension= (fun _ldh _v -> Lwt.return (Rresult.R.error_msgf "Impossible to resolver [%s:%s]" _ldh _v)) }

let rdwr =
  { Colombe.Sigs.rd= (fun flow buf off len ->
        let fiber =
          let open Lwt.Infix in
          Lwt_unix.read flow buf off len >>= fun len ->
          Fmt.epr "-> read %d byte(s): %S.\n%!" len (Bytes.sub_string buf off len) ;
          Lwt.return len in
        Lwt_scheduler.inj fiber)
  ; Colombe.Sigs.wr= (fun flow buf off len ->
        let buf = Bytes.unsafe_of_string buf in
        let fiber =
          let open Lwt.Infix in
          Lwt_unix.write flow buf off len >>= fun _len -> Lwt.return () in
        Lwt_scheduler.inj fiber) }

let load_file filename =
  let open Rresult in
  Bos.OS.File.read filename >>= fun contents ->
  R.ok (Cstruct.of_string contents)

let cert =
  let open Rresult in
  load_file (Fpath.v "ptt.pem") >>= fun raw ->
  X509.Certificate.decode_pem raw

let cert = Rresult.R.get_ok cert

let private_key =
  let open Rresult in
  load_file (Fpath.v "ptt.key") >>= fun raw ->
  X509.Private_key.decode_pem raw >>= fun (`RSA v) -> R.ok v

let private_key = Rresult.R.get_ok private_key

let info =
  { Relay.domain= Colombe.Domain.(v domain [ a "x25519"; a "net" ])
  ; Relay.ipv4= Ipaddr.V4.any
  ; Relay.tls= Tls.Config.server ~certificates:(`Single ([ cert ], private_key)) ~authenticator:X509.Authenticator.null () (* lol *)
  ; Relay.size= 0x1000000L }

let pp_edn ppf = function
  | Unix.ADDR_UNIX v -> Fmt.pf ppf "<unix:%s>" v
  | Unix.ADDR_INET (inet, p) -> Fmt.pf ppf "<inet:%s:%d>" (Unix.string_of_inet_addr inet) p

let loop_server sockaddr resolver server =
  let open Lwt.Infix in
  let master = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.bind master sockaddr >>= fun () ->
  Fmt.pr ">>> server on %a.\n%!" pp_edn sockaddr ;
  let go socket edn () =
    Fmt.epr ">>> accept from %a.\n%!" pp_edn edn ;
    Relay.accept rdwr socket resolver server >>= function
    | Ok () -> Lwt_unix.close socket
    | Error err ->
      Fmt.epr "[ERROR]: from %a: %a.\n%!" pp_edn edn Relay.pp_error err ;
      Lwt_unix.close socket in
  let rec loop () =
    Lwt_unix.accept master >>= fun (socket, edn) ->
    Lwt.async (go socket edn) ; Lwt.pause () >>= loop in
  Lwt_unix.listen master 40 ; loop ()

module Lwt_io : Sage_lwt.SYSCALL with type path = string = struct
  type path = string
  type t = string * Lwt_unix.file_descr

  open Lwt.Infix

  let open_ro path =
    Lwt_unix.(openfile path [ O_RDONLY ] 0o644) >>= fun fd ->
    Lwt.return (path, fd)
  let open_wo path =
    Lwt_unix.(openfile path [ O_WRONLY; O_CREAT ] 0o644) >>= fun fd ->
    Lwt.return (path, fd)

  let length (path, _) =
    let open Lwt.Infix in
    Lwt_unix.stat path >>= fun stat ->
    Lwt.return (Int64.of_int stat.Lwt_unix.st_size)

  let read (_, fd) buf ~off ~len =
    Lwt_unix.read fd buf off len

  let write (_, fd) buf ~off ~len =
    Lwt_unix.write fd buf off len

  let close (_, fd) = Lwt_unix.close fd
end

let record_to_file path consumer =
  let consumer = Sage.v <.> Sage_lwt.inj <.> consumer in
  let open Sage in
  let rec go () =
    let* v = consumer () in
    match v with
    | None -> close
    | Some (buf, off, len) ->
      let* _len = write (Bytes.unsafe_of_string buf) ~off ~len in
      go () in
  let* () = open_file wo ~path in go ()

let loop_logic messaged =
  let open Lwt.Infix in
  let go key queue consumer () =
    let path = Fmt.strf "%04Ld.mail" (Ptt.Messaged.id key) in
    Sage_lwt.run (module Lwt_io) Sage.closed (record_to_file path consumer) >>= fun (s, ()) ->
    match Sage.is_closed s with
    | Some Sage.Refl.Refl ->
      Relay.Md.close queue
    | None ->
      Fmt.epr ">>> %s is not properly closed.\n%!" path ;
      Relay.Md.close queue in
  let rec loop () =
    Relay.Md.await messaged >>= fun () ->
    Relay.Md.pop messaged >>= function
    | None -> Lwt.pause () >>= loop
    | Some (key, queue, consumer) ->
      Lwt.async (go key queue consumer) ; Lwt.pause () >>= loop in
  loop ()

let fiber r sockaddr =
  let server = Relay.create ~info resolver in
  let messaged = Relay.messaged server in
  Lwt.join [ loop_server sockaddr r server
           ; loop_logic messaged
           ; Nocrypto_entropy_lwt.initialize () ]

let r = Dns_client_lwt.create ~clock:(Mtime.to_uint64_ns <.> Mtime_clock.now) ()

let sockaddr =
  let { Unix.h_addr_list; _ } = Unix.gethostbyname (Unix.gethostname ()) in
  Unix.ADDR_INET (h_addr_list.(0), 4242)

let () = Lwt_main.run (fiber r sockaddr)

