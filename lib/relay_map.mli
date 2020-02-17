(** The {i relay-map} allows the user to map an user from a
    specific domain to some others mailboxes. Like:

    Any incoming emails to john@doe.org will be send to:
    {ul
    {- john@gmail.com}
    {- john@wanadoo.fr}}

    To be able to do that, the user must create a {i relay-map} with
    {!empty}: [empty ~postmaster ~domain:"doe.org"] and fills the map
    with [add ~local:"john" "john@gmail.com" |> add ~local:"john" "john@wanadoo.fr"]. *)

type t
(** The type of the map. *)

val postmaster : t -> Emile.mailbox
(** [postmaster m] returns the {i postmaster} of the map which {b is} the postmaster
   of {!domain}'s [m]. *)

val domain : t -> [ `host ] Domain_name.t
(** [domain m] returns the domain handled by [m]. *)

val empty : postmaster:Emile.mailbox -> domain:[ `host ] Domain_name.t -> t
(** [empty ~postmaster ~domain] creates a map which handles the given [domain]. *)

val add : local:Emile.local -> Emile.mailbox -> t -> t
(** [add ~local mailbox m] appends a new deliver mailbox to [local] into [m]. *)

val recipients : local:Emile.local -> t -> Colombe.Forward_path.t list
(** [recipients ~local m] returns all associated mailboxes to [local] in [m]. *)

val all : t -> Colombe.Forward_path.t list
(** [all m] returns all deliver mailboxes registered into [m]. *)

val expand
  :  t
  -> Aggregate.unresolved_elt Aggregate.By_domain.t
  -> Aggregate.resolved_elt Aggregate.By_ipaddr.t
  -> (Aggregate.unresolved_elt Aggregate.By_domain.t * Aggregate.resolved_elt Aggregate.By_ipaddr.t)
