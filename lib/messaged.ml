open Sigs
open Colombe
module Ke = Ke.Rke.Weighted

type from = Reverse_path.t * (string * string option) list
type recipient = Forward_path.t * (string * string option) list

type key = {
    domain_from: Domain.t
  ; from: from
  ; recipients: recipient list
  ; id: int64
}

let domain_from {domain_from; _} = domain_from
let from {from; _} = from
let recipients {recipients; _} = recipients
let id {id; _} = id
let v ~domain_from ~from ~recipients id = {domain_from; from; recipients; id}

let pp ppf key =
  Fmt.pf ppf
    "{ @[<hov>domain_from= %a;@ from= %a;@ recipients= @[<hov>%a@];@ id= \
     %Ld;@] }"
    Domain.pp key.domain_from Reverse_path.pp (fst key.from)
    Fmt.(Dump.list Forward_path.pp)
    (List.map fst key.recipients)
    key.id

let equal_recipients a b =
  let a = List.sort Forward_path.compare a in
  let b = List.sort Forward_path.compare b in
  let rec go a b =
    match a, b with
    | _ :: _, [] -> false
    | [], _ :: _ -> false
    | a :: ar, b :: br ->
      let res = Forward_path.equal a b in
      if res then go ar br else false
    | [], [] -> true in
  go a b

let equal a b =
  Domain.equal a.domain_from b.domain_from
  && Reverse_path.equal (fst a.from) (fst b.from)
  && equal_recipients (List.map fst a.recipients) (List.map fst b.recipients)
  && a.id = b.id

module type S = sig
  type +'a s
  type queue
  type 'a producer = 'a option -> unit s
  type 'a consumer = unit -> 'a option s
  type chunk = string * int * int
  type t

  val create : unit -> t
  val close : queue -> unit s
  val push : ?chunk:int -> t -> key -> chunk producer s
  val await : t -> unit s
  val pop : t -> (key * queue * chunk consumer) option s
  val broadcast : t -> unit
end

module Make (Scheduler : SCHEDULER) (IO : IO with type 'a t = 'a Scheduler.s) =
struct
  type +'a s = 'a IO.t

  open IO

  let ( >>= ) = bind

  type queue = {
      q: (char, Bigarray.int8_unsigned_elt) Ke.t
    ; m: Mutex.t
    ; c: Condition.t
    ; f: bool ref
  }

  let blit_to_bytes src src_off dst dst_off len =
    Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len

  let blit_of_string src src_off dst dst_off len =
    Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

  (* XXX(dinosaure): preferred one writer / one reader *)
  let pipe_of_queue ?(chunk = 0x1000) queue =
    if chunk <= 0 then
      Fmt.invalid_arg "stream_of_queue: chunk must be bigger than 0"

    ; let close = ref false in
      let mutex = Mutex.create () in
      let condition = Condition.create () in

      let consumer () =
        Mutex.lock mutex >>= fun () ->
        let rec wait () =
          if Ke.is_empty queue && not !close then
            Condition.wait condition mutex >>= wait
          else return () in
        wait () >>= fun () ->
        let len = min (Ke.length queue) chunk in

        if len = 0 && !close then (Mutex.unlock mutex ; return None)
        else
          let buf = Bytes.create chunk in
          Ke.N.keep_exn queue ~blit:blit_to_bytes ~length:Bytes.length ~off:0
            ~len buf
          ; Ke.N.shift_exn queue len
          ; Mutex.unlock mutex
          ; return (Some (Bytes.unsafe_to_string buf, 0, len)) in

      let rec producer = function
        | None ->
          Mutex.lock mutex >>= fun () ->
          close := true
          ; Condition.broadcast condition
          ; Mutex.unlock mutex
          ; return ()
        | Some (buf, off, len) as v -> (
          Mutex.lock mutex >>= fun () ->
          if !close then (Mutex.unlock mutex ; return ())
          else
            match
              Ke.N.push queue ~blit:blit_of_string ~length:String.length ~off
                ~len buf
            with
            | None ->
              Condition.signal condition ; Mutex.unlock mutex ; producer v
            | Some _ ->
              Condition.signal condition ; Mutex.unlock mutex ; return ()) in
      {q= queue; m= mutex; c= condition; f= close}, producer, consumer

  let close queue =
    Mutex.lock queue.m >>= fun () ->
    queue.f := true
    ; Mutex.unlock queue.m
    ; return ()

  type 'a producer = 'a option -> unit IO.t
  type 'a consumer = unit -> 'a option IO.t
  type chunk = string * int * int

  type t = {
      q: (key * queue * chunk consumer) Queue.t
    ; m: Mutex.t
    ; c: Condition.t
  }

  let create () =
    {q= Queue.create (); m= Mutex.create (); c= Condition.create ()}

  let push ?chunk t key =
    let queue, _ = Ke.create ~capacity:0x1000 Bigarray.Char in
    let queue, producer, consumer = pipe_of_queue ?chunk queue in
    Mutex.lock t.m >>= fun () ->
    Queue.push (key, queue, consumer) t.q
    ; Condition.signal t.c
    ; Mutex.unlock t.m
    ; return producer

  let await t =
    Mutex.lock t.m >>= fun () ->
    let rec await () =
      if Queue.is_empty t.q then Condition.wait t.c t.m >>= await else return ()
    in
    await () >>= fun () -> Mutex.unlock t.m ; return ()

  let pop t =
    Mutex.lock t.m >>= fun () ->
    try
      let key, queue, consumer = Queue.pop t.q in
      Mutex.unlock t.m
      ; return (Some (key, queue, consumer))
    with _exn -> Mutex.unlock t.m ; return None

  let broadcast t = Condition.broadcast t.c
end
