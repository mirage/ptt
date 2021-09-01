module Make
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Resolver : Ptt.Sigs.RESOLVER with type +'a io = 'a Lwt.t)
    (Stack : Mirage_stack.V4V6) : sig
  val fiber :
       ?stop:Lwt_switch.t
    -> port:int
    -> Stack.t
    -> Resolver.t
    -> Mirage_crypto_pk.Rsa.priv * Dkim.unsigned Dkim.dkim
    -> Ptt.Relay_map.t
    -> Ptt.Logic.info
    -> unit Lwt.t
end
