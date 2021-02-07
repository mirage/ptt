module Make
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Resolver : Ptt.Sigs.RESOLVER with type +'a io = 'a Lwt.t)
    (StackV4 : Mirage_stack.V4) : sig
  val fiber :
       port:int
    -> StackV4.t
    -> Resolver.t
    -> Ptt.Relay_map.t
    -> Ptt.Logic.info
    -> unit Lwt.t
end
