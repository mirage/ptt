open Colombe.State
open Colombe

module Value : sig
  include module type of Logic.Value

  type decoder = Decoder.decoder
  type encoder = Encoder.encoder

  val encode : encoder -> 'x send -> 'x -> (unit, [> Reply.Encoder.error ]) t
  val decode : decoder -> 'x recv -> ('x, [> Request.Decoder.error ]) t
end

module Monad : module type of State.Scheduler (Context) (Value)

type context = Context.t

type error =
  [ `Protocol of Value.error
  | `Too_many_bad_commands
  | `No_recipients
  | `Too_many_recipients ]

val pp_error : error Fmt.t

type info = Logic.info = {
    domain: [ `host ] Domain_name.t
  ; ipv4: Ipaddr.V4.t
  ; tls: Tls.Config.server
  ; zone: Mrmime.Date.Zone.t
  ; size: int64
}

type submission = Logic.submission = {
    from: Messaged.from
  ; recipients: (Forward_path.t * (string * string option) list) list
  ; domain_from: Domain.t
}

val m_properly_close_and_fail :
     context
  -> ?code:int
  -> message:string
  -> ([> error ] as 'err)
  -> ('v, 'err) Colombe.State.t

val m_politely_close : context -> ([> `Quit ], [> error ]) Colombe.State.t

val m_submission :
     context
  -> domain_from:Domain.t
  -> Mechanism.t list
  -> ( [> `Quit | `Authentication of Domain.t * Mechanism.t ]
     , [> error ] )
     Colombe.State.t

val m_relay :
     context
  -> domain_from:Domain.t
  -> ([> `Quit | `Submission of submission ], [> error ]) Colombe.State.t

val m_mail : context -> (unit, [> error ]) Colombe.State.t
val m_end : context -> ([> `Quit ], [> error ]) Colombe.State.t

val m_relay_init :
     context
  -> info
  -> ([> `Quit | `Submission of submission ], [> error ]) Colombe.State.t

val m_submission_init :
     context
  -> info
  -> Mechanism.t list
  -> ( [> `Quit | `Authentication of Domain.t * Mechanism.t ]
     , [> error ] )
     Colombe.State.t
