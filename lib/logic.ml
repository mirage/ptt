open Colombe
open Rresult

module Value = struct
  type helo = Domain.t
  type mail_from = Reverse_path.t * (string * string option) list
  type rcpt_to = Forward_path.t * (string * string option) list
  type auth = PLAIN
  type pp_220 = string list
  type pp_221 = string list
  type pp_235 = string list
  type pp_250 = string list
  type tp_354 = string list
  type tn_454 = string list
  type pn_503 = string list
  type pn_504 = string list
  type pn_530 = string list
  type pn_535 = string list
  type pn_554 = string list
  type pn_555 = string list
  type code = int * string list

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
    | PP_235 : pp_235 send
    | TP_354 : tp_354 send
    | TN_454 : tn_454 send
    | PN_503 : pn_503 send
    | PN_504 : pn_504 send
    | PN_530 : pn_530 send
    | PN_535 : pn_535 send
    | PN_554 : pn_554 send
    | PN_555 : pn_555 send
    | Payload : string send
    | Code : code send

  type error =
    [ Request.Decoder.error
    | Reply.Encoder.error
    | `Too_many_bad_commands
    | `No_recipients
    | `Too_many_recipients
    | `Invalid_recipients ]

  let pp_error ppf = function
    | #Reply.Encoder.error as err -> Reply.Encoder.pp_error ppf err
    | #Request.Decoder.error as err -> Request.Decoder.pp_error ppf err
    | `Too_many_bad_commands -> Fmt.string ppf "Too many bad commands"
    | `No_recipients -> Fmt.string ppf "No recipients"
    | `Too_many_recipients -> Fmt.string ppf "Too many recipients"
    | `Invalid_recipients -> Fmt.string ppf "Invalid recipients"
end

module type MONAD = sig
  type context
  type error

  val bind :
       ('a, 'err) Colombe.State.t
    -> f:('a -> ('b, 'err) Colombe.State.t)
    -> ('b, 'err) Colombe.State.t

  val ( let* ) :
       ('a, 'err) Colombe.State.t
    -> ('a -> ('b, 'err) Colombe.State.t)
    -> ('b, 'err) Colombe.State.t

  val ( >>= ) :
       ('a, 'err) Colombe.State.t
    -> ('a -> ('b, 'err) Colombe.State.t)
    -> ('b, 'err) Colombe.State.t

  val encode :
       context
    -> 'a Value.send
    -> 'a
    -> (context -> ('b, ([> `Protocol of error ] as 'err)) Colombe.State.t)
    -> ('b, 'err) Colombe.State.t

  val decode :
       context
    -> 'a Value.recv
    -> (   context
        -> 'a
        -> ('b, ([> `Protocol of error ] as 'err)) Colombe.State.t)
    -> ('b, 'err) Colombe.State.t

  val send :
       context
    -> 'a Value.send
    -> 'a
    -> (unit, [> `Protocol of error ]) Colombe.State.t

  val recv :
    context -> 'a Value.recv -> ('a, [> `Protocol of error ]) Colombe.State.t

  val return : 'a -> ('a, 'err) Colombe.State.t
  val fail : 'err -> ('a, 'err) Colombe.State.t

  val reword_error :
       ('err0 -> 'err1)
    -> ('v, 'err0) Colombe.State.t
    -> ('v, 'err1) Colombe.State.t

  val error_msgf :
    ('a, Format.formatter, unit, ('b, [> R.msg ]) Colombe.State.t) format4 -> 'a
end

let () = Colombe.Request.Decoder.add_extension "STARTTLS"
let () = Colombe.Request.Decoder.add_extension "AUTH"

(* XXX(dinosaure): shoud be ok! *)

type info = {
    domain: [ `host ] Domain_name.t
  ; ipv4: Ipaddr.V4.t
  ; tls: Tls.Config.server
  ; zone: Mrmime.Date.Zone.t
  ; size: int64
}

type submission = {
    from: Messaged.from
  ; recipients: (Forward_path.t * (string * string option) list) list
  ; domain_from: Domain.t
}

module Make (Monad : MONAD) = struct
  let src = Logs.Src.create "logic"

  module Log = (val Logs.src_log src : Logs.LOG)

  let politely ~domain ~ipv4 =
    Fmt.strf "%a at your service, [%s]" Domain_name.pp domain
      (Ipaddr.V4.to_string ipv4)

  let m_properly_close_and_fail ctx ?(code = 554) ~message err =
    let open Monad in
    let* () = send ctx Value.Code (code, [message]) in
    Error err

  let m_politely_close ctx =
    let open Monad in
    let* () = send ctx Value.PP_221 ["Bye, buddy!"] in
    (* TODO(dinosaure): properly close when we use a STARTTLS context.
       Currently fixed into [SMTP]: when we want to [`Quit], we properly
       close the TLS connection. *)
    return `Quit

  let m_relay ctx ~domain_from =
    let open Monad in
    let reset = ref 0 and bad = ref 0 in
    let rec mail_from () =
      if !reset >= 25 || !bad >= 25 then
        m_properly_close_and_fail ctx ~message:"You reached the limit buddy!"
          `Too_many_bad_commands
      else
        let* command = recv ctx Value.Any in
        match command with
        | `Quit -> m_politely_close ctx
        | `Mail from ->
          let* () = send ctx Value.PP_250 ["Ok, buddy!"] in
          recipients ~from []
        | `Reset ->
          incr reset
          ; send ctx Value.PP_250 ["Yes buddy!"] >>= fun () -> mail_from ()
        | v ->
          incr bad
          ; Log.warn (fun m ->
                m "%a sended a bad command: %a" Domain.pp domain_from Request.pp
                  v)
          ; send ctx Value.PN_503 ["Command out of sequence"] >>= fun () ->
            mail_from ()
    and recipients ~from acc =
      if !reset >= 25 || !bad >= 25 then
        m_properly_close_and_fail ctx ~message:"You reached the limit buddy!"
          `Too_many_bad_commands
      else
        let* command = recv ctx Value.Any in
        match command with
        | `Data -> (
          match acc with
          | [] ->
            m_properly_close_and_fail ctx ~message:"No recipients"
              `No_recipients
          | acc ->
            let recipients = List.rev acc in
            return (`Submission {from; recipients; domain_from}))
        | `Recipient v ->
          (* XXX(dinosaure): the minimum number of recipients that MUST be
             buffered is 100 recipients. *)
          if List.length acc < 100 then
            send ctx Value.PP_250 ["Ok, buddy!"] >>= fun () ->
            recipients ~from (v :: acc)
          else
            send ctx Value.Code (452, ["Too many recipients, buddy! "])
            >>= fun () -> fail `Too_many_recipients
        | `Reset ->
          incr reset
          ; send ctx Value.PP_250 ["Yes buddy!"] >>= fun () -> mail_from ()
        | `Quit -> m_politely_close ctx
        | v ->
          incr bad
          ; Log.warn (fun m ->
                m "%a sended a bad command: %a" Domain.pp domain_from Request.pp
                  v)
          ; send ctx Value.PN_503 ["Command out of sequence"] >>= fun () ->
            recipients ~from acc in
    mail_from ()

  exception Unrecognized_authentication

  let m_submission ctx ~domain_from ms =
    let open Monad in
    let reset = ref 0 and bad = ref 0 in
    let rec auth_0 () =
      if !reset >= 25 || !bad >= 25 then
        m_properly_close_and_fail ctx ~message:"You reached the limit buddy!"
          `Too_many_bad_commands
      else
        let* command = recv ctx Value.Any in
        match command with
        | `Verb ("AUTH", [mechanism]) -> (
          try
            let mechanism = Mechanism.of_string_exn mechanism in
            if List.exists (Mechanism.equal mechanism) ms then
              return (`Authentication (domain_from, mechanism))
            else raise Unrecognized_authentication
          with Invalid_argument _ | Unrecognized_authentication ->
            incr bad
            ; send ctx Value.PN_504 ["Unrecognized authentication!"] >>= auth_0)
        | `Verb ("AUTH", []) ->
          incr bad
          ; let* () = send ctx Value.PN_555 ["Syntax error, buddy!"] in
            auth_0 ()
        | `Reset ->
          incr reset
          ; send ctx Value.PP_250 ["Yes buddy!"] >>= auth_0
        | `Quit -> m_politely_close ctx
        | v ->
          incr bad
          ; Log.warn (fun m ->
                m " %a sended a bad command: %a" Domain.pp domain_from
                  Request.pp v)
          ; let* () =
              send ctx Value.PN_530 ["Authentication required, buddy!"]
            in
            auth_0 () in
    auth_0 ()

  let m_mail ctx =
    let open Monad in
    send ctx Value.TP_354 ["Ok buddy! Finish it with <crlf>.<crlf>"]

  let m_end ctx =
    let open Monad in
    let* () = send ctx Value.PP_250 ["Mail sended, buddy!"] in
    let* () = recv ctx Value.Quit in
    m_politely_close ctx

  let m_relay_init ctx info =
    let open Monad in
    let* domain_from = recv ctx Value.Helo in
    let* () =
      send ctx Value.PP_250
        [
          politely ~domain:info.domain ~ipv4:info.ipv4; "8BITMIME"; "SMTPUTF8"
        ; Fmt.strf "SIZE %Ld" info.size
        ]
    in
    m_relay ctx ~domain_from

  let m_submission_init ctx info ms =
    let open Monad in
    let* domain_from = recv ctx Value.Helo in
    let* () =
      send ctx Value.PP_250
        [
          politely ~domain:info.domain ~ipv4:info.ipv4; "8BITMIME"; "SMTPUTF8"
        ; Fmt.strf "SIZE %Ld" info.size
        ; Fmt.strf "AUTH %a" Fmt.(list ~sep:(const string " ") Mechanism.pp) ms
        ]
    in
    m_submission ctx ~domain_from ms
end
