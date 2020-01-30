open Colombe.State
open Colombe

module Value : sig
  type helo = Domain.t
  type mail_from = Reverse_path.t * (string * string option) list
  type rcpt_to = Forward_path.t * (string * string option) list
  type auth = PLAIN

  type pp_220 = string list
  type pp_221 = string list
  type pp_250 = string list
  type tp_354 = string list
  type tn_454 = string list
  type pn_503 = string list
  type pn_504 = string list
  type pn_530 = string list
  type pn_554 = string list
  type code = int * string list

  type error =
    [ Request.Decoder.error
    | Reply.Encoder.error
    | `Too_many_bad_commands
    | `No_recipients
    | `Too_many_recipients
    | `Invalid_recipients ]

  val pp_error : error Fmt.t

  type 'x recv =
    | Helo : helo recv
    | Mail_from : mail_from recv
    | Rcpt_to : rcpt_to recv
    | Data : unit recv
    | Dot : unit recv
    | Quit : unit recv
    | Auth : auth recv
    | Payload : string recv
    | Any : Request.t recv

  type 'x send =
    | PP_220 : pp_220 send
    | PP_221 : pp_221 send
    | PP_250 : pp_250 send
    | TP_354 : tp_354 send
    | TN_454 : tn_454 send
    | PN_503 : pn_503 send
    | PN_504 : pn_504 send
    | PN_530 : pn_530 send
    | PN_554 : pn_554 send
    | Payload : string send
    | Code : code send

  val encode_without_tls : Encoder.encoder -> 'x send -> 'x -> (unit, [> Reply.Encoder.error ]) t
  val decode_without_tls : Decoder.decoder -> 'x recv -> ('x, [> Request.Decoder.error ]) t
end

module Value_with_tls : module type of Sendmail_with_tls.Make_with_tls(Value)
module Monad : module type of State.Scheduler(Sendmail_with_tls.Context_with_tls)(Value_with_tls)

type context = Sendmail_with_tls.Context_with_tls.t

type info =
  { domain : [ `host ] Domain_name.t
  ; ipv4 : Ipaddr.V4.t
  ; tls : Tls.Config.server
  ; zone : Mrmime.Date.Zone.t
  ; size : int64 }

type submission =
  { authenticated : [ `Anonymous | `As of Emile.local ]
  ; from : Messaged.from
  ; recipients : (Forward_path.t * (string * string option) list) list
  ; domain_from : Domain.t
  ; tls : bool }

type error = Value_with_tls.error

val pp_error : error Fmt.t

val m_properly_close_and_fail : context -> ?code:int -> message:string -> error -> (unit, error) Colombe.State.t
val m_politely_close : context -> ([> `Quit ], error) Colombe.State.t

val m_relay : context -> domain_from:Domain.t -> ([> `Quit | `Submission of submission ], error) Colombe.State.t
val m_mail  : context -> (unit, error) Colombe.State.t
val m_end   : context -> ([> `Quit ], error) Colombe.State.t
val m_init  : context -> info -> ([> `Quit | `Submission of submission ], error) Colombe.State.t
