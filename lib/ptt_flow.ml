let src = Logs.Src.create "ptt.flow"

module Log = (val Logs.src_log src)
open Colombe.Sigs
open Colombe.State
open Colombe
open Lwt.Infix

let ( <.> ) f g = fun x -> f (g x)

module Lwt_scheduler = Sigs.Make (Lwt)

let lwt_bind x f =
  let open Lwt_scheduler in
  inj (prj x >>= (prj <.> f))

let lwt =
  {Sigs.bind= lwt_bind; return= (fun x -> Lwt_scheduler.inj (Lwt.return x))}

exception Rdwr of string

module Rdwr (Flow : Mirage_flow.S) = struct
  let blit0 src src_off dst dst_off len =
    let dst = Cstruct.of_bigarray ~off:dst_off ~len dst in
    Cstruct.blit src src_off dst 0 len

  let blit1 src src_off dst dst_off len =
    Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len

  let failwith pp = function
    | Ok v -> Lwt.return v
    | Error err -> Lwt.fail (Rdwr (Fmt.str "%a" pp err))

  type t = {queue: (char, Bigarray.int8_unsigned_elt) Ke.Rke.t; flow: Flow.flow}

  let make flow = {flow; queue= Ke.Rke.create ~capacity:0x800 Bigarray.char}

  let recv flow payload p_off p_len =
    if Ke.Rke.is_empty flow.queue then (
      Flow.read flow.flow >>= failwith Flow.pp_error >>= function
      | `Eof -> Lwt.return 0
      | `Data res ->
        Ke.Rke.N.push flow.queue ~blit:blit0 ~length:Cstruct.length res;
        let len = min p_len (Ke.Rke.length flow.queue) in
        Ke.Rke.N.keep_exn flow.queue ~blit:blit1 ~length:Bytes.length ~off:p_off
          ~len payload;
        Ke.Rke.N.shift_exn flow.queue len;
        Lwt.return len)
    else
      let len = min p_len (Ke.Rke.length flow.queue) in
      Ke.Rke.N.keep_exn flow.queue ~blit:blit1 ~length:Bytes.length ~off:p_off
        ~len payload;
      Ke.Rke.N.shift_exn flow.queue len;
      Lwt.return len

  let send flow payload p_off p_len =
    let cs = Cstruct.of_string payload ~off:p_off ~len:p_len in
    Flow.write flow.flow cs >>= failwith Flow.pp_write_error
end

module Make (Flow : Mirage_flow.S) = struct
  module Flow' = Rdwr (Flow)

  type flow = Flow'.t

  let rdwr =
    let open Lwt_scheduler in
    let rd flow buf off len =
      Flow'.recv flow buf off len >>= function
      | 0 -> Lwt.return `End
      | len -> Lwt.return (`Len len) in
    let rd flow buf off len = inj (rd flow buf off len)
    and wr flow buf off len = inj (Flow'.send flow buf off len) in
    {Colombe.Sigs.rd; wr}

  let run : type s flow.
         s impl
      -> (flow, s) rdwr
      -> flow
      -> ('a, 'err) t
      -> (('a, 'err) result, s) io =
   fun {bind; return} rdwr flow m ->
    let ( >>= ) = bind in

    let rec go = function
      | Read {buffer; off; len; k} ->
        rdwr.rd flow buffer off len >>= fun v -> go (k v)
      | Write {buffer; off; len; k} ->
        rdwr.wr flow buffer off len >>= fun () -> go (k len)
      | Return v -> return (Ok v)
      | Error err -> return (Error err : ('a, 'err) result) in
    go m

  let make = Flow'.make

  let run flow m =
    Lwt.catch (fun () -> Lwt_scheduler.prj (run lwt rdwr flow m)) @@ function
    | Rdwr msg -> Lwt.return_error (`Flow msg)
    | exn -> Lwt.reraise exn
end
