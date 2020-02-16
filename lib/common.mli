open Sigs
open Rresult

module Make
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
    (Flow : FLOW with type 'a s = 'a IO.t)
    (Resolver : RESOLVER with type 'a s = 'a IO.t)
    (Random : RANDOM with type 'a s = 'a IO.t)
  : sig
    type 'w resolver
    type 'g random = ?g:'g -> bytes -> unit IO.t
    type 'a consumer = 'a option -> unit IO.t

    val ( >>= ) : 'a IO.t -> ('a -> 'b IO.t) -> 'b IO.t
    val ( >>? ) : ('a, 'err) result IO.t -> ('a -> ('b, 'err) result IO.t) -> ('b, 'err) result IO.t

    val resolver : Resolver.t resolver
    val generate : Random.g random
    val scheduler : Scheduler.t Colombe.Sigs.impl
    val rdwr : (Flow.t, Scheduler.t) Colombe.Sigs.rdwr

    val run
      :  Flow.t
      -> ('a, 'err) Colombe.State.t
      -> ('a, [> `Error of 'err
              |  `Connection_close ]) result IO.t

    val recipients_are_reachable
      :  ipv4:Ipaddr.V4.t
      -> Resolver.t
      -> Colombe.Forward_path.t list
      -> bool IO.t

    val receive_mail
      :  ?limit:int
      -> Flow.t
      -> 'ctx
      -> ('ctx -> (string, 'err) Colombe.State.t)
      -> (string * int * int) consumer
      -> (unit, [> `Error of 'err
                |  `Connection_close
                |  `Too_big_data ]) result IO.t

    val resolve_recipients
      :  domain:[ `host ] Domain_name.t
      -> Resolver.t
      -> Relay_map.t
      -> Colombe.Forward_path.t list
      -> ([ `Domain of [ `host ] Domain_name.t * Mxs.t
          | `Ipaddr of Ipaddr.t ] * Aggregate.resolved_elt) list IO.t
  end
