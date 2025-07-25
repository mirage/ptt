(** {1:A DKIM signer as a SMTP server.}

    This module implements a server which signs incoming emails with a private
    RSA key. It re-sends emails with the computed DKIM field. *)

type dkim = Dkim.unsigned Dkim.t

type signer =
  | DKIM of {dkim: dkim; pk: Dkim.key}
  | ARC of {seal: Arc.Sign.seal; msgsig: dkim; pks: Arc.key * Arc.key option}

module Make
    (Stack : Tcpip.Stack.V4V6)
    (Dns_client : Dns_client_mirage.S)
    (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow) : sig
  val job :
       ?limit:int
    -> ?stop:Lwt_switch.t
    -> ?destination:Ipaddr.t
    -> locals:Ptt_map.t
    -> port:int
    -> tls:Tls.Config.client
    -> info:Ptt_common.info
    -> Stack.TCP.t
    -> Dns_client.t
    -> Happy_eyeballs.t
    -> signer
    -> unit Lwt.t
end
