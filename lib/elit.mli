module Make
    (Stack : Tcpip.Stack.V4V6)
    (Dns_client : Dns_client_mirage.S)
    (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow) : sig
  type 'k t

  type 'k iter =
       (Ptt_map.local -> 'k Digestif.t -> Emile.mailbox list -> unit Lwt.t)
    -> unit Lwt.t

  val v :
       ?g:Mirage_crypto_rng.g
    -> ?mechanisms:Ptt.Mechanism.t list
    -> postmaster:Emile.mailbox
    -> ?forward_granted:Ipaddr.Prefix.t list
    -> 'k Digestif.hash
    -> 'k iter
    -> Ipaddr.t
    -> 'k t Lwt.t

  val job :
       ?stop:Lwt_switch.t
    -> 'k t
    -> info:Ptt_common.info
    -> ?submission:Tls.Config.server
    -> ?relay:Tls.Config.server
    -> Stack.TCP.t
    -> Dns_client.t
    -> Happy_eyeballs.t
    -> unit Lwt.t
end
