module Make
    (Pclock : Mirage_clock.PCLOCK)
    (StackV4 : Mirage_stack.V4)
    (Md : Ptt.Messaged.S with type 'a s = 'a Lwt.t) : sig
  val transmit :
       info:Ptt.Logic.info
    -> StackV4.t
    -> Ptt.Messaged.key * Md.queue * Md.chunk Md.consumer
    -> ([ `Domain of [ `host ] Domain_name.t * Ptt.Mxs.t | `Ipaddr of Ipaddr.t ]
       * Ptt.Aggregate.resolved_elt)
       list
    -> unit Lwt.t
end
