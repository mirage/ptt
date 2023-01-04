(** {1:Submission server with authentication.}

    This module implements a server which lets users to send an email under an
    identity. It implements an authentication system from a map of users with
    their (hashed) passwords. The server must have a {!val:Tls.Config.server},
    otherwise, we raise an [Invalid_argument] (and submission server must
    wrap the SMTP protocol with the Transport Security Layer). *)

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
    -> Random.g option
    -> 'k Digestif.hash
    -> Ptt.Logic.info
    -> (Ptt_tuyau.Lwt_backend.Lwt_scheduler.t, 'k) Ptt.Authentication.t
    -> Ptt.Mechanism.t list
    -> unit Lwt.t
end
