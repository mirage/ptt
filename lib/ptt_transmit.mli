module Make
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Mirage_stack.V4V6)
    (Md : Ptt.Messaged.S with type 'a s = 'a Lwt.t) : sig
  val transmit :
       info:Ptt.Logic.info
    -> tls:Tls.Config.client
    -> Stack.t
    -> Ptt.Messaged.key * Md.queue * Md.chunk Md.consumer
    -> ([ `Domain of [ `host ] Domain_name.t * Ptt.Mxs.t | `Ipaddr of Ipaddr.t ]
       * Ptt.Aggregate.resolved_elt)
       list
    -> unit Lwt.t
end
