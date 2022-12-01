open Colombe.State
open Colombe

let ( <.> ) f g x = f (g x)

module Value = struct
  include Logic.Value

  type decoder = Decoder.decoder
  type encoder = Encoder.encoder

  let encode : type a. encoder -> a send -> a -> (unit, [> Encoder.error ]) t =
   fun encoder w v ->
    let fiber : a send -> [> Encoder.error ] Encoder.state = function
      | Payload ->
        let k encoder =
          Encoder.write v encoder
          ; Encoder.write "\r\n" encoder
          ; Encoder.flush (fun _ -> Encoder.Done) encoder in
        Encoder.safe k encoder
      | PP_220 -> Reply.Encoder.response (`PP_220 v) encoder
      | PP_221 -> Reply.Encoder.response (`PP_221 v) encoder
      | PP_235 -> Reply.Encoder.response (`Other (235, v)) encoder
      | PP_250 -> Reply.Encoder.response (`PP_250 v) encoder
      | TP_334 -> Reply.Encoder.response (`Other (334, v)) encoder
      | TP_354 -> Reply.Encoder.response (`TP_354 v) encoder
      | TN_454 -> Reply.Encoder.response (`Other (454, v)) encoder
      | PN_503 -> Reply.Encoder.response (`PN_503 v) encoder
      | PN_504 -> Reply.Encoder.response (`PN_504 v) encoder
      | PN_530 -> Reply.Encoder.response (`Other (530, v)) encoder
      | PN_535 -> Reply.Encoder.response (`Other (535, v)) encoder
      | PN_554 -> Reply.Encoder.response (`PN_554 v) encoder
      | PN_555 -> Reply.Encoder.response (`Other (555, v)) encoder
      | Code ->
        let code, txts = v in
        Reply.Encoder.response (`Other (code, txts)) encoder in
    let rec go = function
      | Encoder.Done -> Return ()
      | Encoder.Write {continue; buffer; off; len} ->
        Write {k= go <.> continue; buffer; off; len}
      | Encoder.Error err -> Error err in
    (go <.> fiber) w

  let decode : type a. decoder -> a recv -> (a, [> Decoder.error ]) t =
   fun decoder w ->
    let k : Request.t -> (a, [> Decoder.error ]) t =
     fun v ->
      match w, v with
      | Helo, `Hello v -> Return v
      | Mail_from, `Mail v -> Return v
      | Rcpt_to, `Recipient v -> Return v
      | Data, `Data -> Return ()
      | Dot, `Data_end -> Return ()
      | Quit, `Quit -> Return ()
      | Auth, `Verb ("AUTH", ["PLAIN"]) -> Return PLAIN
      | Payload, `Payload v -> Return v
      | Payload, `Data_end -> Return "."
      | Any, v -> Return v
      | _, v ->
        let v = Fmt.to_to_string Request.pp v in
        Error (`Invalid_command v) in
    let rec go = function
      | Decoder.Done v -> k v
      | Decoder.Read {buffer; off; len; continue} ->
        Read {k= go <.> continue; buffer; off; len}
      | Decoder.Error {error; _} -> Error error in
    go (Request.Decoder.request ~relax:true decoder)
end

module Monad = struct
  type context = Context.t

  include State.Scheduler (Context) (Value)
end

type context = Context.t

type error =
  [ `Protocol of Value.error
  | `Too_many_bad_commands
  | `No_recipients
  | `Too_many_recipients ]

let pp_error ppf = function
  | `Protocol err -> Value.pp_error ppf err
  | `Too_many_bad_commands -> Fmt.string ppf "Too many bad commands"
  | `No_recipients -> Fmt.string ppf "No recipients"
  | `Too_many_recipients -> Fmt.string ppf "Too many recipients"

type info = Logic.info = {
    domain: [ `host ] Domain_name.t
  ; ipaddr: Ipaddr.t
  ; tls: Tls.Config.server
  ; zone: Mrmime.Date.Zone.t
  ; size: int64
}

type submission = Logic.submission = {
    from: Messaged.from
  ; recipients: (Forward_path.t * (string * string option) list) list
  ; domain_from: Domain.t
}

include Logic.Make (Monad)

let m_submission_init ctx info ms =
  let open Monad in
  let* () = send ctx Value.PP_220 [Domain_name.to_string info.Logic.domain] in
  m_submission_init ctx info ms

let m_relay_init ctx info =
  let open Monad in
  let* () = send ctx Value.PP_220 [Domain_name.to_string info.Logic.domain] in
  m_relay_init ctx info
