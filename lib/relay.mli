open Colombe.Sigs
open Sigs

module Make
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
  : sig
    module Md : module type of Messaged.Make(Scheduler)(IO)

    type 'r resolver =
      { gethostbyname : 'a. 'r -> [ `host ] Domain_name.t -> (Ipaddr.V4.t, [> Rresult.R.msg ] as 'a) result IO.t
      ; extension : 'a. string -> string -> (Ipaddr.t, [> Rresult.R.msg ] as 'a) result IO.t }
    type 'r server
    type info = SMTP.info =
      { domain : Colombe.Domain.t
      ; ipv4 : Ipaddr.V4.t
      ; tls : Tls.Config.server
      ; size : int64 }

    type error

    val pp_error : error Fmt.t

    val create : info:info -> 'r resolver -> 'r server
    val messaged : 'r server -> Md.t
    val accept : ('flow, Scheduler.t) rdwr -> 'flow -> 'r -> 'r server -> (unit, error) result IO.t
  end
