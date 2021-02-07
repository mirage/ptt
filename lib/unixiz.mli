module Make (Flow : Mirage_flow.S) : sig
  include Ptt.Sigs.FLOW with type +'a io = 'a Lwt.t

  val make : Flow.flow -> t
end
