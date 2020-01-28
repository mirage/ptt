open Colombe.State
open Colombe

let ( <.> ) f g = fun x -> f (g x)

module Value = struct
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

  let pp_error ppf = function
    | #Reply.Encoder.error as err -> Reply.Encoder.pp_error ppf err
    | #Request.Decoder.error as err -> Request.Decoder.pp_error ppf err
    | `Too_many_bad_commands -> Fmt.string ppf "Too many bad commands"
    | `No_recipients -> Fmt.string ppf "No recipients"
    | `Too_many_recipients -> Fmt.string ppf "Too many recipients"
    | `Invalid_recipients -> Fmt.string ppf "Invalid recipients"

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

  let encode_without_tls
    : type a. Encoder.encoder -> a send -> a -> (unit, [> Encoder.error ]) t
    = fun encoder w v ->
      let fiber : a send -> [> Encoder.error ] Encoder.state = function
        | Payload ->
          let k encoder =
            Encoder.write v encoder ;
            Encoder.write "\r\n" encoder ;
            Encoder.flush (fun _ -> Encoder.Done) encoder in
          Encoder.safe k encoder
        | PP_220 -> Reply.Encoder.response (`PP_220 v) encoder
        | PP_221 -> Reply.Encoder.response (`PP_221 v) encoder
        | PP_250 -> Reply.Encoder.response (`PP_250 v) encoder
        | TP_354 -> Reply.Encoder.response (`TP_354 v) encoder
        | TN_454 -> Reply.Encoder.response (`Other (454, v)) encoder
        | PN_503 -> Reply.Encoder.response (`PN_503 v) encoder
        | PN_504 -> Reply.Encoder.response (`PN_504 v) encoder
        | PN_530 -> Reply.Encoder.response (`Other (530, v)) encoder
        | PN_554 -> Reply.Encoder.response (`PN_554 v) encoder
        | Code ->
          let code, txts = v in
          Reply.Encoder.response (`Other (code, txts)) encoder in
      let rec go = function
        | Encoder.Done -> Return ()
        | Encoder.Write { continue; buffer; off; len; } ->
          Write { k= go <.> continue; buffer; off; len; }
        | Encoder.Error err -> Error err in
      (go <.> fiber) w

  let decode_without_tls
    : type a. Decoder.decoder -> a recv -> (a, [> Decoder.error ]) t
    = fun decoder w ->
      let k : Request.t -> (a, [> Decoder.error ]) t = fun v -> match w, v with
        | Helo, `Hello v -> Return v
        | Mail_from, `Mail v -> Return v
        | Rcpt_to, `Recipient v -> Return v
        | Data, `Data -> Return ()
        | Dot, `Data_end -> Return ()
        | Quit, `Quit -> Return ()
        | Auth, `Verb ("AUTH", [ "PLAIN" ]) -> Return PLAIN
        | Payload, `Payload v -> Return v
        | Payload, `Data_end -> Return "."
        | Any, v -> Return v
        | _, v ->
          let v = Fmt.to_to_string Request.pp v in
          Error (`Invalid_command v) in
      let rec go = function
        | Decoder.Done v -> k v
        | Decoder.Read { buffer; off; len; continue; } ->
          Read { k= go <.> continue; buffer; off; len; }
        | Decoder.Error { error; _ } -> Error error in
      go (Request.Decoder.request ~relax:true decoder)
end

let src = Logs.Src.create "smtp"
module Log = (val Logs.src_log src : Logs.LOG)
module Value_with_tls = Sendmail_with_tls.Make_with_tls(Value)
module Monad = State.Scheduler(Sendmail_with_tls.Context_with_tls)(Value_with_tls)

type context = Sendmail_with_tls.Context_with_tls.t

type info =
  { domain : [ `host ] Domain_name.t
  ; ipv4 : Ipaddr.V4.t
  ; tls : Tls.Config.server
  ; size : int64 }

type submission =
  { authenticated : [ `Anonymous | `As of Emile.local ]
  ; from : Messaged.from
  ; recipients : (Forward_path.t * (string * string option) list) list
  ; domain_from : Domain.t
  ; tls : bool }

type error = Value_with_tls.error

let pp_error = Value_with_tls.pp_error

let politely ~domain ~ipv4 =
  Fmt.strf "%a at your service, [%s]"
    Domain_name.pp domain (Ipaddr.V4.to_string ipv4)

let m_properly_close_and_fail ctx ?(code= 554) ~message err =
  let open Monad in
  let* () = send ctx Value.Code (code, [ message ]) in
  fail err

let m_politely_close ctx =
  let open Monad in
  let* () = send ctx Value.PP_220 [ "Bye, buddy!" ] in
  let* () = Value_with_tls.close (Sendmail_with_tls.Context_with_tls.encoder ctx) in
  return `Quit

let m_relay ctx ~domain_from =
  let open Monad in
  let reset = ref 0 and bad = ref 0 in
  let rec mail_from () =
    let* from = recv ctx Value.Mail_from in
    let* () = send ctx Value.PP_250 [ "Ok, buddy!" ] in
    recipients ~from []
  and recipients ~from acc =
    if !reset >= 25 || !bad >= 25
    then
      m_properly_close_and_fail ctx
        ~message:"You reached the limit buddy!"
        (`Protocol `Too_many_bad_commands)
    else
      let* command = recv ctx Value.Any in
      match command with
      | `Data ->
        ( match acc with
          | [] ->
            m_properly_close_and_fail ctx
              ~message:"No recipients"
              (`Protocol `No_recipients)
          | acc ->
            let recipients = List.rev acc in
            return (`Submission { authenticated= `Anonymous
                                ; from
                                ; recipients
                                ; domain_from
                                ; tls= Sendmail_with_tls.Context_with_tls.tls ctx }) )
      | `Recipient v ->
        (* XXX(dinosaure): the minimum number of recipients that MUST be
           buffered is 100 recipients. *)
        if List.length acc < 100
        then send ctx Value.PP_250 [ "Ok buddy!" ] >>= fun () ->
          recipients ~from (v :: acc)
        else send ctx Value.Code (452, [ "Too many recipients, buddy! "]) >>= fun () ->
          fail (`Protocol `Too_many_recipients)
      | `Reset ->
        incr reset ;
        send ctx Value.PP_250 [ "Yes buddy!" ] >>= fun () ->
        mail_from ()
      | `Quit -> m_politely_close ctx
      | v ->
        incr bad ;
        Log.warn (fun m -> m "%a sended a bad command: %a"
                     Domain.pp domain_from Request.pp v) ;
        send ctx Value.PN_503 [ "Command out of sequence" ] >>= fun () ->
        recipients ~from acc in
  mail_from ()

let m_mail ctx =
  let open Monad in
  send ctx Value.TP_354 [ "Ok buddy! Finish it with <crlf>.<crlf>" ]

let m_end ctx =
  let open Monad in
  let* () = send ctx Value.PP_250 [ "Mail sended, buddy!" ] in
  let* () = recv ctx Value.Quit in
  m_politely_close ctx

let m_init_with_tls ctx s =
  let open Monad in
  let* domain_from =
    (* send ctx Value.PP_220 [ Domain.to_string s.domain ] >>= fun () -> *)
    recv ctx Value.Helo in
  let* () = send ctx Value.PP_250 [ politely ~domain:s.domain ~ipv4:s.ipv4
                                  ; "8BITMIME"
                                  ; "SMTPUTF8"
                                  ; Fmt.strf "SIZE %Ld" s.size ] in
  m_relay ctx ~domain_from

let () = Colombe.Request.Decoder.add_extension "STARTTLS"
(* XXX(dinosaure): shoud be ok! *)

let m_init ctx s =
  let open Monad in
  let* _from_domain = send ctx Value.PP_220 [ Domain_name.to_string s.domain ] >>= fun () -> recv ctx Value.Helo in
  let* () = send ctx Value.PP_250
      [ politely ~domain:s.domain ~ipv4:s.ipv4
      ; "8BITMIME"
      ; "SMTPUTF8"
      ; "STARTTLS"
      ; Fmt.strf "SIZE %Ld" s.size ] in
  let reset = ref 0 and bad = ref 0 in
  let rec go () =
    if !reset >= 25 && !bad >= 25
    then m_properly_close_and_fail ctx
        ~message:"You reached the limit buddy!"
        (`Protocol `Too_many_bad_commands)
    else
      let* command = recv ctx Value.Any in
      match command with
      | `Verb ("STARTTLS", []) (* | `Payload "STARTTLS" *) ->
        let* () = send ctx Value.PP_220 [ "Go ahead buddy!" ] in
        let decoder = Sendmail_with_tls.Context_with_tls.decoder ctx in
        let* () = Value_with_tls.starttls_as_server decoder s.tls in
        m_init_with_tls ctx s
      | `Reset ->
        incr reset ;
        let* () = send ctx Value.PP_250 [ "Yes buddy!" ] in
        go ()
      | `Quit -> m_politely_close ctx
      | _ ->
        incr bad ;
        let* () = send ctx Value.PN_530 [ "Must issue a STARTTLS command first." ] in
        go () in
  go ()
