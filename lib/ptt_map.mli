type t
type local = [ `Dot_string of string list | `String of string ]

val postmaster : t -> Emile.mailbox
val empty : postmaster:Emile.mailbox -> t
val add : local:local -> Emile.mailbox -> t -> unit
val exists_as_sender : Colombe.Reverse_path.t -> info:Ptt_common.info -> t -> bool
val recipients : local:local -> t -> Colombe.Forward_path.t list
val all : t -> Colombe.Forward_path.t list

val expand :
     info:Ptt_common.info
  -> t
  -> Colombe.Forward_path.t list
  -> Colombe.Forward_path.t list
