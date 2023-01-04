(** {1:A simple SMTP relay.}

    This module implements a server which re-send incoming emails to their
    real destinations (including the {i map} which can alter destinations). For
    instance, the relay-map can define that for an email to [foo@bar.com], the
    real destination is [foo@gmail.com]. *)

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
    -> ?locals:Ptt.Relay_map.t
    -> port:int
    -> tls:Tls.Config.client
    -> Stack.TCP.t
    -> Resolver.t
    -> Ptt.Logic.info
    -> unit Lwt.t
end
