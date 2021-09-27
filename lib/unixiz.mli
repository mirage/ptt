module Make (Flow : Mirage_flow.S) : sig
  include Ptt.Sigs.FLOW with type +'a io = 'a Lwt.t

  val make : Flow.flow -> t
  val close : t -> unit Lwt.t

  type flow = t

  val input : flow -> bytes -> int -> int -> int Lwt.t
end
