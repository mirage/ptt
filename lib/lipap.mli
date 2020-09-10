module Make
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Resolver : Ptt.Sigs.RESOLVER with type +'a io = 'a Lwt.t)
    (StackV4 : Mirage_stack.V4)
  : sig
    val fiber
      :  StackV4.t
      -> Resolver.t
      -> Random.g
      -> 'k Digestif.hash
      -> StackV4.t Conduit_mirage_tcp.configuration
      -> Ptt.Relay_map.t
      -> Ptt.Logic.info
      -> (Ptt_tuyau.Lwt_backend.Lwt_scheduler.t, 'k) Ptt.Authentication.t
      -> Ptt.Mechanism.t list
      -> unit Lwt.t
  end
