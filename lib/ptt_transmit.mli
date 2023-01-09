module Make
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    (Md : Ptt.Messaged.S with type 'a s = 'a Lwt.t) : sig
  type pool =
    (bytes * bytes * (char, Bigarray.int8_unsigned_elt) Ke.Rke.t) Lwt_pool.t

  val transmit :
       pool:pool
    -> info:Ptt.Logic.info
    -> tls:Tls.Config.client
    -> Stack.TCP.t
    -> ?emitter:Colombe.Reverse_path.t
    -> Ptt.Messaged.key * Md.queue * Md.chunk Md.consumer
    -> ([ `Domain of [ `host ] Domain_name.t * Ptt.Mxs.t | `Ipaddr of Ipaddr.t ]
       * Ptt.Aggregate.resolved_elt)
       list
    -> unit Lwt.t
  (** [transmit ~pool ~info ~tls tcpv4v6 (key, queue, consumer) recipients] tries
      to send an email to [recipients]. *)
end
