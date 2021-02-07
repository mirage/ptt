module Lwt_backend : module type of Lwt_backend
open Lwt_backend

module type FLOW = Ptt.Sigs.FLOW with type +'a io = 'a Lwt.t

module Make (StackV4 : Mirage_stack.V4) : sig
  module Flow : sig
    include Ptt.Sigs.FLOW with type +'a io = 'a Lwt.t

    val make : StackV4.TCPV4.flow -> t
  end

  module TLSFlow : sig
    include Ptt.Sigs.FLOW with type +'a io = 'a Lwt.t

    val server : StackV4.TCPV4.flow -> Tls.Config.server -> t Lwt.t
    val client : StackV4.TCPV4.flow -> Tls.Config.client -> t Lwt.t
  end

  val sendmail :
       info:Ptt.Logic.info
    -> ?tls:Tls.Config.client
    -> StackV4.t
    -> Ipaddr.V4.t
    -> Colombe.Reverse_path.t
    -> (string * int * int, Lwt_scheduler.t) Sendmail.stream
    -> Colombe.Forward_path.t list
    -> (unit, [> `Flow of StackV4.TCPV4.error | `Msg of string ]) result Lwt.t
end

module Server (Time : Mirage_time.S) (StackV4 : Mirage_stack.V4) : sig
  type service

  val init : port:int -> StackV4.t -> service Lwt.t

  val serve_when_ready :
       ?timeout:int64
    -> ?stop:Lwt_switch.t
    -> handler:(StackV4.TCPV4.flow -> unit Lwt.t)
    -> service
    -> [ `Initialized of unit Lwt.t ]
end
