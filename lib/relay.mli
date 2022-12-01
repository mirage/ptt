open Rresult
open Sigs

module Make
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
    (Flow : FLOW with type 'a io = 'a IO.t)
    (Resolver : RESOLVER with type 'a io = 'a IO.t)
    (Random : RANDOM with type 'a io = 'a IO.t) : sig
  module Md : module type of Messaged.Make (Scheduler) (IO)

  type server

  type info = SMTP.info = {
      domain: [ `host ] Domain_name.t
    ; ipaddr: Ipaddr.t
    ; tls: Tls.Config.server
    ; zone: Mrmime.Date.Zone.t
    ; size: int64
  }

  val info : server -> info

  type error

  val pp_error : error Fmt.t

  val resolve_recipients :
       domain:[ `host ] Domain_name.t
    -> Resolver.t
    -> Relay_map.t
    -> Colombe.Forward_path.t list
    -> ([ `Domain of [ `host ] Domain_name.t * Mxs.t | `Ipaddr of Ipaddr.t ]
       * Aggregate.resolved_elt)
       list
       IO.t

  val create : info:info -> server
  val messaged : server -> Md.t

  val accept :
       ?encoder:(unit -> bytes)
    -> ?decoder:(unit -> bytes)
    -> ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t)
    -> ipaddr:Ipaddr.t
    -> Flow.t
    -> Resolver.t
    -> server
    -> (unit, error) result IO.t
end
