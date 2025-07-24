type 'k cfg

module Make
    (Stack : Tcpip.Stack.V4V6)
    (Dns_client : Dns_client_mirage.S)
    (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow) : sig
  type 'k iter =
       (Ptt_map.local -> 'k Digestif.t -> Emile.mailbox list -> unit Lwt.t)
    -> unit Lwt.t

  val v :
       info:Ptt_common.info
    -> ?g:Mirage_crypto_rng.g
    -> ?mechanisms:Ptt.Mechanism.t list
    -> postmaster:Emile.mailbox
    -> ?forward_granted:Ipaddr.Prefix.t list
    -> ?with_arc:bool
    -> ?mx_destination:Ipaddr.t
    -> ?on_admin:
         (sender:Colombe.Reverse_path.t -> string Lwt_stream.t -> unit Lwt.t)
    -> 'k Digestif.hash
    -> 'k iter
    -> Ipaddr.t
    -> 'k cfg Lwt.t

  val job :
       ?stop:Lwt_switch.t
    -> 'k cfg
    -> ?submission:Tls.Config.server
    -> ?relay:Tls.Config.server
    -> Stack.TCP.t
    -> Dns_client.t
    -> Happy_eyeballs.t
    -> unit Lwt.t
end
