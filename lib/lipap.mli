(** {1:Submission server with authentication.}

    This module implements a server which lets users to send an email under an
    identity. It implements an authentication system from a map of users with
    their (hashed) passwords. The server must have a {!val:Tls.Config.server},
    otherwise, we raise an [Invalid_argument] (and submission server must
    wrap the SMTP protocol with the Transport Security Layer). *)

module Make
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    (Dns_client : Dns_client_mirage.S)
    (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow) :
sig
  val job :
       ?limit:int
    -> ?stop:Lwt_switch.t
    -> locals:Ptt_map.t
    -> port:int
    -> tls:Tls.Config.client
    -> info:Ptt_common.info
    -> Mirage_crypto_rng.g option
    -> 'k Digestif.hash
    -> Stack.TCP.t
    -> Dns_client.t
    -> Happy_eyeballs.t
    -> 'k Ptt.Authentication.t
    -> Ptt.Mechanism.t list
    -> unit Lwt.t
end
