open Rresult
open Sigs

module Make
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
    (Flow : FLOW with type 'a s = 'a IO.t)
    (Resolver : RESOLVER with type 'a s = 'a IO.t)
    (Random : RANDOM with type 'a s = 'a IO.t)
  : sig
    module Md : module type of Messaged.Make(Scheduler)(IO)

    type 'k server

    type info = SSMTP.info =
      { domain : [ `host ] Domain_name.t
      ; ipv4 : Ipaddr.V4.t
      ; tls : Tls.Config.server
      ; zone : Mrmime.Date.Zone.t
      ; size : int64 }

    val info : 'k server -> info

    type error

    val pp_error : error Fmt.t

    val resolve_recipients
      :  domain:[ `host ] Domain_name.t
      -> Resolver.t
      -> Relay_map.t
      -> Colombe.Forward_path.t list
      -> ([ `Domain of [ `host ] Domain_name.t * Mxs.t
          | `Ipaddr of Ipaddr.t ] * Aggregate.resolved_elt) list IO.t

    val create : info:info -> authenticator:(Scheduler.t, 'k) Authentication.t -> Mechanism.t list -> 'k server
    val messaged : 'k server -> Md.t
    val accept : Flow.t -> Resolver.t -> Random.g -> 'k Digestif.hash -> 'k server -> (unit, error) result IO.t
  end
