open Colombe.Sigs

module type SCHEDULER = X

module type MUTEX = sig
  type +'a fiber
  type t

  val create : unit -> t
  val lock : t -> unit fiber
  val unlock : t -> unit
end

module type CONDITION = sig
  type +'a fiber
  type mutex
  type t

  val create : unit -> t
  val wait : t -> mutex -> unit fiber
  val signal : t -> unit
  val broadcast : t -> unit
end

module type IO = sig
  type +'a t

  module Mutex : MUTEX with type 'a fiber = 'a t

  module Condition :
    CONDITION with type 'a fiber = 'a t and type mutex = Mutex.t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type RESOLVER = sig
  type t
  type +'a io

  val gethostbyname :
    t -> [ `host ] Domain_name.t -> (Ipaddr.V4.t, [> Rresult.R.msg ]) result io

  val getmxbyname :
       t
    -> [ `host ] Domain_name.t
    -> (Dns.Rr_map.Mx_set.t, [> Rresult.R.msg ]) result io

  val extension :
    t -> string -> string -> (Ipaddr.V4.t, [> Rresult.R.msg ]) result io
end

module type RANDOM = sig
  type g
  type +'a io

  val generate : ?g:g -> bytes -> unit io
end

module type FLOW = sig
  type t
  type +'a io

  val recv : t -> bytes -> int -> int -> int io
  val send : t -> string -> int -> int -> unit io
end
