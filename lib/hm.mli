(** {1:SPF verifier as a server.}

    This module implements a server which verifies SPF values from income
    emails. As a server/unikernel, it must be in front of internet. Indeed, it
    uses the IP address of incoming emails and verify if the domain of the
    sender allows this IP address. *)

module Make
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Resolver : Ptt.Sigs.RESOLVER with type +'a io = 'a Lwt.t)
    (Stack : Tcpip.Stack.V4V6)
    (DNS : Dns_client_mirage.S
             with type Transport.stack = Stack.t
              and type 'a Transport.io = 'a Lwt.t) : sig
  val fiber :
       ?limit:int
    -> ?stop:Lwt_switch.t
    -> ?locals:Ptt.Relay_map.t
    -> port:int
    -> tls:Tls.Config.client
    -> Stack.TCP.t
    -> Resolver.t
    -> Ptt.Logic.info
    -> DNS.t
    -> unit Lwt.t
end
