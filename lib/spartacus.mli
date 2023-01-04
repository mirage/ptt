(** {1:A filter as a SMTP server.}

    This module implements a server which {b annotes} incoming emails with a
    field to recognizes them as a spam or not. It re-sends emails with this
    field then. *)

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
