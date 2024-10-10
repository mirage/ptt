module Make (Flow : Mirage_flow.S) : sig
  type flow

  val make : Flow.flow -> flow

  val run :
       flow
    -> ('a, ([> `Flow of string ] as 'err)) Colombe.State.t
    -> ('a, 'err) result Lwt.t
end
