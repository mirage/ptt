module Make
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Resolver : Ptt.Sigs.RESOLVER with type +'a io = 'a Lwt.t)
    (Stack : Mirage_protocols.TCP with type ipaddr = Ipaddr.t) : sig
  val fiber :
       ?limit:int
    -> ?stop:Lwt_switch.t
    -> port:int
    -> tls:Tls.Config.client
    -> Stack.t
    -> Resolver.t
    -> Random.g option
    -> 'k Digestif.hash
    -> Ptt.Relay_map.t
    -> Ptt.Logic.info
    -> (Ptt_tuyau.Lwt_backend.Lwt_scheduler.t, 'k) Ptt.Authentication.t
    -> Ptt.Mechanism.t list
    -> unit Lwt.t
end
