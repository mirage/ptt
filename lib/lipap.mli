module Make
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Resolver : Ptt.Sigs.RESOLVER with type +'a io = 'a Lwt.t)
    (Stack : Tcpip.Stack.V4V6) : sig
  val fiber :
       ?limit:int
    -> ?stop:Lwt_switch.t
    -> port:int
    -> tls:Tls.Config.client
    -> Stack.TCP.t
    -> Resolver.t
    -> Random.g option
    -> 'k Digestif.hash
    -> Ptt.Relay_map.t
    -> Ptt.Logic.info
    -> (Ptt_tuyau.Lwt_backend.Lwt_scheduler.t, 'k) Ptt.Authentication.t
    -> Ptt.Mechanism.t list
    -> unit Lwt.t
end
