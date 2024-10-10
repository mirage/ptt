(** {1:SPF verifier as a server.}

    This module implements a server which verifies SPF values from income
    emails. As a server/unikernel, it must be in front of internet. Indeed, it
    uses the IP address of incoming emails and verify if the domain of the
    sender allows this IP address. *)

module Make
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    (Dns_client : Dns_client_mirage.S)
    (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow) : sig
  val job :
       ?limit:int
    -> ?stop:Lwt_switch.t
    -> locals:Ptt_map.t
    -> port:int
    -> tls:Tls.Config.client
    -> info:Ptt_common.info
    -> Stack.TCP.t
    -> Dns_client.t
    -> Happy_eyeballs.t
    -> unit Lwt.t
end
