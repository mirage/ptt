open Sigs
open Rresult

module Make
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
    (Flow : FLOW with type 'a io = 'a IO.t)
    (Resolver : RESOLVER with type 'a io = 'a IO.t)
    (Random : RANDOM with type 'a io = 'a IO.t) : sig
  type 'w resolver
  type 'g random = ?g:'g -> bytes -> unit IO.t
  type 'a consumer = 'a option -> unit IO.t

  val ( >>= ) : 'a IO.t -> ('a -> 'b IO.t) -> 'b IO.t

  val ( >>? ) :
       ('a, 'err) result IO.t
    -> ('a -> ('b, 'err) result IO.t)
    -> ('b, 'err) result IO.t

  val resolver : Resolver.t resolver
  val generate : Random.g random
  val scheduler : Scheduler.t Colombe.Sigs.impl
  val rdwr : (Flow.t, Scheduler.t) Colombe.Sigs.rdwr

  val run :
       Flow.t
    -> ('a, 'err) Colombe.State.t
    -> ('a, [> `Error of 'err | `Connection_close ]) result IO.t
  (** [run flow m] runs [m] on [flow]. [m] is a description of what we will send
      and receive. [flow] is a representation of the {i socket}. *)

  val recipients_are_reachable :
    ipv4:Ipaddr.V4.t -> Resolver.t -> Colombe.Forward_path.t list -> bool IO.t
  (** [recipients_are_reachable ~ipv4 resolver recipients] tries to resolve
      {i MX} record for any domains of recipients. [ipv4] is the {b public} IPv4
      of the current SMTP server to resolve [Postmaster]. *)

  val receive_mail :
       ?limit:int
    -> Flow.t
    -> 'ctx
    -> ('ctx -> (string, 'err) Colombe.State.t)
    -> (string * int * int) consumer
    -> (unit, [> `Error of 'err | `Connection_close | `Too_big_data ]) result
       IO.t
  (** [receive_mail ?limit flow ctx m consumer] runs [m] which gives to us a
      {i payload} from a given [ctx] (with [STARTTLS] or not) and a [flow].
      Then, it transmits these {i payloads} to the [consumer].

      [limit] can protect us to an infinite filler. If we reach the limit,
      [`Too_big_data] is returned. *)

  val resolve_recipients :
       domain:[ `host ] Domain_name.t
    -> Resolver.t
    -> Relay_map.t
    -> Colombe.Forward_path.t list
    -> ([ `Domain of [ `host ] Domain_name.t * Mxs.t | `Ipaddr of Ipaddr.t ]
       * Aggregate.resolved_elt)
       list
       IO.t
  (** [resolve_recipients ~domain resolver map recipients] tries to resolve
      [recipients] with [resolver] and rightly translates recipients found into
      [map] to expected recipients.

      The result is a list of:

      - [`Domain (domain, mxs)] which is an aggregation of all recipients with
        the same [domain]. [mxs] is a set of Mail eXchange services (priority
        and IP address) of the [domain].
      - [`Ipaddr ip] is an aggregation of all recipients with the same IP as a
        domain. *)
end
