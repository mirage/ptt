(** {1:A DKIM signer as a SMTP server.}

    This module implements a server which signs incoming emails with a private
    RSA key. It re-sends emails with the computed DKIM field.
*)

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
    -> Mirage_crypto_pk.Rsa.priv * Dkim.unsigned Dkim.dkim
    -> Ptt.Logic.info
    -> unit Lwt.t
end
