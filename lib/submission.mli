open Rresult
open Sigs

module Make
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
    (Flow : FLOW with type 'a io = 'a IO.t)
    (Resolver : RESOLVER with type 'a io = 'a IO.t)
    (Random : RANDOM with type 'a io = 'a IO.t) : sig
  module Md : module type of Messaged.Make (Scheduler) (IO)

  type 'k server

  type info = SSMTP.info = {
      domain: [ `host ] Domain_name.t
    ; ipv4: Ipaddr.V4.t
    ; tls: Tls.Config.server
    ; zone: Mrmime.Date.Zone.t
    ; size: int64
  }

  val info : 'k server -> info

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

  val create :
       info:info
    -> authenticator:(Scheduler.t, 'k) Authentication.t
    -> Mechanism.t list
    -> 'k server

  val messaged : 'k server -> Md.t

  val accept :
       ipaddr:Ipaddr.t
    -> Flow.t
    -> Resolver.t
    -> Random.g option
    -> 'k Digestif.hash
    -> 'k server
    -> (unit, error) result IO.t
  (** [accept flow resolver random alg server] is a simple SMTP process which
      accepts an incoming email iff the client is authentified. The method to
      safely check the password uses the hash algorithm [alg] and private
      information from the given [server] (see {!create}). If the user is
      correctly authentified, the incoming email is added into the internal
      [server]'s queue.

      The incoming email is accepted only if recipients given by the client are
      reachable {i via} the given [resolver] - see {!Common.Make.recipients_are_reachable}.
      Otherwise, the incoming email is discarded! *)
end
