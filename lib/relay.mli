open Rresult

module Make
    (Stack : Tcpip.Stack.V4V6) : sig
  type server

  type info = SMTP.info =
    { domain: Colombe.Domain.t
    ; ipaddr: Ipaddr.t
    ; tls: Tls.Config.server option
    ; zone: Mrmime.Date.Zone.t
    ; size: int64 }

  val info : server -> info

  type error

  val pp_error : error Fmt.t

  val create :
       info:info
    -> server * (Msgd.key * string Lwt_stream.t * Msgd.result Lwt.u) Lwt_stream.t * (unit -> unit)

  val accept :
       ?encoder:(unit -> bytes)
    -> ?decoder:(unit -> bytes)
    -> ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t)
    -> ipaddr:Ipaddr.t
    -> Stack.TCP.flow
    -> 'dns
    -> 'dns Ptt_common.resolver
    -> server
    -> (unit, error) result Lwt.t
end
