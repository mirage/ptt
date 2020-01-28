module Mutex = struct
  type 'a fiber = 'a Lwt.t
  type t = Lwt_mutex.t

  let create () = Lwt_mutex.create ()
  let lock t = Lwt_mutex.lock t
  let unlock t = Lwt_mutex.unlock t
end

module Condition = struct
  type 'a fiber = 'a Lwt.t
  type mutex = Mutex.t
  type t = unit Lwt_condition.t
  (* XXX(dinosaure): unix backend does not give to us a chance
     to pass a value. *)

  let create () = Lwt_condition.create ()
  let wait t mutex = Lwt_condition.wait ~mutex t
  let signal t = Lwt_condition.signal t ()
  let broadcast t = Lwt_condition.broadcast t ()
end

module Lwt_scheduler = Colombe.Sigs.Make(struct type +'a t = 'a Lwt.t end)

module Lwt_io = struct
  type +'a t = 'a Lwt.t

  module Mutex = Mutex
  module Condition = Condition

  let bind = Lwt.bind
  let return = Lwt.return
end

let lwt =
  let open Lwt.Infix in
  let open Lwt_scheduler in
  { Colombe.Sigs.bind= (fun x f -> inj (prj x >>= fun x -> prj (f x)))
  ; Colombe.Sigs.return= (fun x -> inj (Lwt.return x)) }
