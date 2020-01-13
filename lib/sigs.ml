open Colombe.Sigs

module type SCHEDULER = X

module type MUTEX = sig
  type +'a fiber
  type t

  val create : unit -> t
  val lock : t -> unit fiber
  val unlock : t -> unit
end

module type FUTURE = sig
  type +'a fiber
  type 'a t

  val wait : 'a t -> 'a fiber
  val peek : 'a t -> 'a option
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
  module Condition : CONDITION with type 'a fiber = 'a t and type mutex = Mutex.t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end
