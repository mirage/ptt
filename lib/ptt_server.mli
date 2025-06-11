module Make (Stack : Tcpip.Stack.V4V6) : sig
  type service

  val init : port:int -> Stack.TCP.t -> service Lwt.t

  val serve_when_ready :
       ?stop:Lwt_switch.t
    -> handler:(Stack.TCP.flow -> unit Lwt.t)
    -> service
    -> [ `Initialized of unit Lwt.t ]
end
