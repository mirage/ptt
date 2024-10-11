open Colombe

type from = Reverse_path.t * (string * string option) list
type recipient = Forward_path.t * (string * string option) list

type key =
  { domain_from: Domain.t
  ; from: from
  ; recipients: recipient list
  ; id: int64
  ; ip: Ipaddr.t }

let domain_from {domain_from; _} = domain_from
let from {from; _} = from
let recipients {recipients; _} = recipients
let id {id; _} = id
let ipaddr {ip; _} = ip

let key ~domain_from ~from ~recipients ~ipaddr:ip id =
  {domain_from; from; recipients; id; ip}

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
  && Ipaddr.compare a.ip b.ip = 0

(*
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
*)

let src = Logs.Src.create "ptt.messaged"

module Log = (val Logs.src_log src)

type t = (key * string Lwt_stream.t) Lwt_stream.t

(*
let blit_to_bytes src src_off dst dst_off len =
  Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len

let blit_of_string src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

(* XXX(dinosaure): preferred one writer / one reader *)
let pipe_of_queue ?(chunk = 0x1000) queue =
  if chunk <= 0 then
    Fmt.invalid_arg "stream_of_queue: chunk must be bigger than 0";

  let close = ref false in
  let mutex = Mutex.create () in
  let condition = Condition.create () in

  let consumer () =
    let rec wait () =
      if Ke.is_empty queue && not !close
      then Lwt_condition.wait ~mutex condition >>= wait
      else Lwt.return_unit in
    Lwt_mutex.with_lock mutex @@ fun () ->
    let* () = wait () in
    let len = min (Ke.length queue) chunk in

    if len = 0 && !close then Lwt.return_none
    else
      let buf = Bytes.create chunk in
      Log.debug (fun m -> m "Transmit %d byte(s) from the client." len);
      Ke.N.keep_exn queue ~blit:blit_to_bytes ~length:Bytes.length ~off:0 ~len
        buf;
      Ke.N.shift_exn queue len;
      Lwt.return_some (Bytes.unsafe_to_string buf, 0, len) in

  let rec producer = function
    | None ->
      Log.debug (fun m ->
          m "The client finished the transmission of the message.");
      Lwt_mutex.with_lock mutex @@ fun () ->
      close := true;
      Condition.broadcast condition ();
      Lwt.return_unit
    | Some (buf, off, len) as v ->
      let* next = Lwt_mutex.with_lock mutex @@ fun () ->
        if !close then Lwt.return_unit
        else
          let length = String.length in
          match Ke.N.push queue ~blit:blit_of_string ~length ~off ~len buf with
          | None ->
            Lwt_condition.signal condition ();
            Lwt.pause () >>= fun () -> Lwt.return `Retry
          | Some _ ->
            Lwt_condition.signal condition ();
            Lwt.return `Stop in
      match next with
      | `Retry -> producer v
      | `Stop -> Lwt.return_unit
  in
  {q= queue; m= mutex; c= condition; f= close}, producer, consumer

let close queue =
  Mutex.lock queue.m >>= fun () ->
  queue.f := true;
  Mutex.unlock queue.m;
  return ()

type 'a producer = 'a option -> unit IO.t
type 'a consumer = unit -> 'a option IO.t
type chunk = string * int * int

type t =
  { q: (key * queue * chunk consumer) Queue.t
  ; m: Lwt_mutex.t
  ; c: unit Lwt_condition.t }

let create () =
  {q= Queue.create (); m= Lwt_mutex.create (); c= Lwt_condition.create ()}

let push ?chunk t key =
  let queue, _ = Ke.create ~capacity:0x1000 Bigarray.Char in
  let queue, producer, consumer = pipe_of_queue ?chunk queue in
  let ( let* ) = Lwt.bind in
  let* () = Lwt_mutex.with_lock t.m @@ fun () ->
    Queue.push (key, queue, consumer) t.q;
    Lwt_condition.signal t.c ();
    Lwt.return_unit in
  return producer

let await t =
  let rec await () =
    if Queue.is_empty t.q
    then Lwt_condition.wait ~mutex:t.m t.c >>= await
    else Lwt.return_unit in
  Lwt_mutex.lock t.m await

let pop t =
  Lwt_mutex.with_lock t.m @@ fun () ->
  match Queue.pop t.q with
  | key, queue, consumer -> Lwt.return_some (key, queue, consumer)
  | exception _ -> Lwt.return_none

let broadcast t =
  Lwt_mutex.with_lock t.m @@ fun () ->
  Lwt_condition.broadcast t.c ();
  Lwt.return_unit
*)
