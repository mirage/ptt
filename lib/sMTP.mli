open Colombe.State
open Colombe

module Value : sig
  include module type of Logic.Value

  val encode_without_tls :
    Encoder.encoder ->
    'x send ->
    'x ->
    (unit, [> `Protocol of SSMTP.Value.error ]) t

  val decode_without_tls :
    Decoder.decoder -> 'x recv -> ('x, [> `Protocol of SSMTP.Value.error ]) t
end

module Value_with_tls :
    module type of Sendmail_with_starttls.Make_with_tls (Value)

module Monad :
    module type of
      State.Scheduler (Sendmail_with_starttls.Context_with_tls) (Value_with_tls)

type context = Sendmail_with_starttls.Context_with_tls.t

type error =
  [ `Tls of
    [ `Protocol of Value.error
    | `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure ]
  | `Protocol of
    [ `Protocol of Value.error
    | `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure ]
  | `No_recipients
  | `Invalid_recipients
  | `Too_many_bad_commands
  | `Too_many_recipients ]

val pp_error : error Fmt.t

type info = Logic.info = {
  domain : [ `host ] Domain_name.t;
  ipv4 : Ipaddr.V4.t;
  tls : Tls.Config.server;
  zone : Mrmime.Date.Zone.t;
  size : int64;
}

type submission = Logic.submission = {
  from : Messaged.from;
  recipients : (Forward_path.t * (string * string option) list) list;
  domain_from : Domain.t;
}

val m_properly_close_and_fail :
  context ->
  ?code:int ->
  message:string ->
  ([> error ] as 'err) ->
  (unit, 'err) Colombe.State.t

val m_politely_close : context -> ([> `Quit ], [> error ]) Colombe.State.t

val m_submission :
  context ->
  domain_from:Domain.t ->
  Mechanism.t list ->
  ( [> `Quit | `Authentication of Domain.t * Mechanism.t ],
    [> error ] )
  Colombe.State.t

val m_relay :
  context ->
  domain_from:Domain.t ->
  ([> `Quit | `Submission of submission ], [> error ]) Colombe.State.t

val m_mail : context -> (unit, [> error ]) Colombe.State.t

val m_end : context -> ([> `Quit ], [> error ]) Colombe.State.t

val m_relay_init :
  context ->
  info ->
  ([> `Quit | `Submission of submission ], [> error ]) Colombe.State.t

val m_submission_init :
  context ->
  info ->
  Mechanism.t list ->
  ( [> `Quit | `Authentication of Domain.t * Mechanism.t ],
    [> error ] )
  Colombe.State.t
