module Lwt_backend : module type of Lwt_backend
open Lwt_backend

module type FLOW = Ptt.Sigs.FLOW with type +'a io = 'a Lwt.t

module Make (Stack : Mirage_protocols.TCP) : sig
  module Flow : sig
    include Ptt.Sigs.FLOW with type +'a io = 'a Lwt.t

    val make : Stack.flow -> t

    type flow = t

    val input : flow -> bytes -> int -> int -> int Lwt.t
  end

  module TLSFlow : sig
    include Ptt.Sigs.FLOW with type +'a io = 'a Lwt.t

    val server : Stack.flow -> Tls.Config.server -> t Lwt.t
    val client : Stack.flow -> Tls.Config.client -> t Lwt.t
    val close : t -> unit Lwt.t
  end

  val sendmail :
       ?encoder:(unit -> bytes)
    -> ?decoder:(unit -> bytes)
    -> ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t)
    -> info:Ptt.Logic.info
    -> tls:Tls.Config.client
    -> Stack.t
    -> Stack.ipaddr
    -> Colombe.Reverse_path.t
    -> (string * int * int, Lwt_scheduler.t) Sendmail.stream
    -> Colombe.Forward_path.t list
    -> ( unit
       , [> `Flow of Stack.error | `STARTTLS_unavailable | `Msg of string ] )
       result
       Lwt.t

  val sendmail_without_tls :
       ?encoder:(unit -> bytes)
    -> ?decoder:(unit -> bytes)
    -> info:Ptt.Logic.info
    -> Stack.t
    -> Stack.ipaddr
    -> Colombe.Reverse_path.t
    -> (string * int * int, Lwt_scheduler.t) Sendmail.stream
    -> Colombe.Forward_path.t list
    -> (unit, [> `Flow of Stack.error | `Msg of string ]) result Lwt.t

  val pp_error :
    [ `Flow of Stack.error | `Msg of string | `STARTTLS_unavailable ] Fmt.t
end

module Server (Time : Mirage_time.S) (Stack : Mirage_protocols.TCP) : sig
  type service

  val init : ?limit:int -> port:int -> Stack.t -> service Lwt.t

  val serve_when_ready :
       ?timeout:int64
    -> ?stop:Lwt_switch.t
    -> handler:(Stack.flow -> unit Lwt.t)
    -> service
    -> [ `Initialized of unit Lwt.t ]
end
