let blit0 src src_off dst dst_off len =
  let dst = Cstruct.of_bigarray ~off:dst_off ~len dst in
  Cstruct.blit src src_off dst 0 len

let blit1 src src_off dst dst_off len =
  Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len

open Rresult
open Lwt.Infix

module Make (Flow : Mirage_flow.S) = struct
  type +'a io = 'a Lwt.t
  type t = {queue: (char, Bigarray.int8_unsigned_elt) Ke.Rke.t; flow: Flow.flow}
  type flow = t

  let make flow = {flow; queue= Ke.Rke.create ~capacity:0x1000 Bigarray.char}

  let failwith pp = function
    | Ok v -> Lwt.return v
    | Error err -> Lwt.fail (Failure (Fmt.str "%a" pp err))

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

  let input flow payload p_off p_len = recv flow payload p_off p_len

  let send flow payload p_off p_len =
    let cs = Cstruct.of_string payload ~off:p_off ~len:p_len in
    Flow.write flow.flow cs >>= failwith Flow.pp_write_error

  let close {flow; _} = Flow.close flow
end
