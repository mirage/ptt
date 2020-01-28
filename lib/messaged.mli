open Sigs
open Colombe

type from = Reverse_path.t * (string * string option) list
type recipient = Forward_path.t * (string * string option) list

type key

val domain_from : key -> Domain.t
val from : key -> from
val recipients : key -> recipient list
val id : key -> int64

val v : domain_from:Domain.t -> from:from -> recipients:recipient list -> int64 -> key

module Make
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
  : sig
    type queue
    type 'a producer = 'a option -> unit IO.t
    type 'a consumer = unit -> 'a option IO.t
    type chunk = string * int * int

    type t
    (** The type of the dispatcher. *)

    val create : unit -> t
    (** [create ()] creates a message dispatcher. *)

    val close : queue -> unit IO.t
    (** [close queue] notifies [queue] to be closed. Then,
       {!producer} will {i eat} anything an {!consumer} will return
       as soon as possible [None]. *)

    val push : ?chunk:int -> t -> key -> chunk producer IO.t
    (** [push ?chunk messaged key] adds a new message into the
       dispatcher and give to the caller the {!producer} to fill
       the message asynchronously. *)

    val await : t -> unit IO.t
    (** [await t] awaits a new message from the dispatcher.

      {b Note.} by design, a server should never stop, [await] can
      infinitely blocks the process as long as there is no message
      available in [t]. *)

    val pop : t -> (key * queue * chunk consumer) option IO.t
    (** [pop t] returns next message available in [t]. It gives
       the identifier, the queue which contains the mail and
       a safe-threaded consumer. *)
  end
