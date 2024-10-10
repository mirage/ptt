(** {1:A filter as a SMTP server.}

    This module implements a server which {b annotes} incoming emails with a
    field to recognizes them as a spam or not. It re-sends emails with this
    field then. *)

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
