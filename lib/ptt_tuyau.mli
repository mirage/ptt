module Lwt_backend : module type of Lwt_backend

open Lwt_backend

module type FLOW = Ptt.Sigs.FLOW with type +'a io = 'a Lwt.t

module Make (StackV4 : Mirage_stack.V4) : sig
  module TCP : module type of Conduit_mirage_tcp.Make (StackV4)

  module Flow : FLOW with type t = TCP.protocol

  val flow :
    (module Conduit_mirage.PROTOCOL
       with type flow = 'flow
        and type endpoint = 'endpoint) ->
    (module FLOW with type t = 'flow)

  val rdwr : (Conduit_mirage.flow, Lwt_scheduler.t) Colombe.Sigs.rdwr

  val sendmail :
    info:Ptt.Logic.info ->
    ?tls:Tls.Config.client ->
    StackV4.t ->
    Ipaddr.V4.t ->
    Colombe.Reverse_path.t ->
    (string * int * int, Lwt_scheduler.t) Sendmail.stream ->
    Colombe.Forward_path.t list ->
    (unit, [> Conduit_mirage.error ]) result Lwt.t
end
