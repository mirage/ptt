module Lwt_backend : module type of Lwt_backend
open Lwt_backend

module type FLOW = Ptt.Sigs.FLOW with type +'a io = 'a Lwt.t

module Make (Stack : Mirage_stack.V4V6) : sig
  module Flow : sig
    include Ptt.Sigs.FLOW with type +'a io = 'a Lwt.t

    val make : Stack.TCP.flow -> t
  end

  module TLSFlow : sig
    include Ptt.Sigs.FLOW with type +'a io = 'a Lwt.t

    val server : Stack.TCP.flow -> Tls.Config.server -> t Lwt.t
    val client : Stack.TCP.flow -> Tls.Config.client -> t Lwt.t
  end

  val sendmail :
       info:Ptt.Logic.info
    -> tls:Tls.Config.client
    -> Stack.t
    -> Ipaddr.t
    -> Colombe.Reverse_path.t
    -> (string * int * int, Lwt_scheduler.t) Sendmail.stream
    -> Colombe.Forward_path.t list
    -> (unit, [> `Flow of Stack.TCP.error | `Msg of string ]) result Lwt.t
end

module Server (Time : Mirage_time.S) (Stack : Mirage_stack.V4V6) : sig
  type service

  val init : port:int -> Stack.t -> service Lwt.t

  val serve_when_ready :
       ?timeout:int64
    -> ?stop:Lwt_switch.t
    -> handler:(Stack.TCP.flow -> unit Lwt.t)
    -> service
    -> [ `Initialized of unit Lwt.t ]
end
