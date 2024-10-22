open Colombe.State
open Colombe

module Value : sig
  include module type of Logic.Value

  val encode_without_tls :
       Encoder.encoder
    -> 'x send
    -> 'x
    -> (unit, error) t

  val decode_without_tls :
    Decoder.decoder -> 'x recv -> ('x, error) t
end

module Value_with_tls :
  module type of Sendmail_with_starttls.Make_with_tls (Value)

module Monad : Logic.MONAD
  with type context = Sendmail_with_starttls.Context_with_tls.t
   and type error = Value_with_tls.error

type context = Sendmail_with_starttls.Context_with_tls.t

type error =
  [ `No_recipients
  | `Protocol of Value_with_tls.error
  | `Too_many_bad_commands
  | `Too_many_recipients
  | `Tls of Value_with_tls.error ]

val pp_error : error Fmt.t

type info = Ptt_common.info = {
    domain: Colombe.Domain.t
  ; ipaddr: Ipaddr.t
  ; tls: Tls.Config.server option
  ; zone: Mrmime.Date.Zone.t
  ; size: int64
}

type email = Logic.email = {
    from: Msgd.from
  ; recipients: (Forward_path.t * (string * string option) list) list
  ; domain_from: Domain.t
}

val m_properly_close_and_fail :
     context
  -> ?code:int
  -> message:string
  -> ([> error ] as 'err)
  -> (unit, 'err) Colombe.State.t

val m_politely_close : context -> ([> `Quit ], [> error ]) Colombe.State.t

val m_submission :
     context
  -> domain_from:Domain.t
  -> Mechanism.t list
  -> ( [> `Quit
       | `Authentication of Domain.t * Mechanism.t
       | `Authentication_with_payload of Domain.t * Mechanism.t * string ]
     , [> error ])
     Colombe.State.t

val m_relay :
     context
  -> domain_from:Domain.t
  -> ([> `Quit | `Send of email ], [> error ]) Colombe.State.t

val m_mail : context -> (unit, [> error ]) Colombe.State.t

val m_end :
     [ `Aborted
     | `Not_enough_memory
     | `Too_big
     | `Failed
     | `Requested_action_not_taken of [ `Temporary | `Permanent ]
     | `Ok ]
  -> context
  -> ([> `Quit ], [> error ]) Colombe.State.t

val m_relay_init :
     context
  -> info
  -> ([> `Quit | `Send of email ], [> error ]) Colombe.State.t

val m_submission_init :
     context
  -> info
  -> Mechanism.t list
  -> ( [> `Quit
       | `Authentication of Domain.t * Mechanism.t
       | `Authentication_with_payload of Domain.t * Mechanism.t * string ]
     , [> error ])
     Colombe.State.t
