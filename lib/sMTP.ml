open Colombe

let ( <.> ) f g x = f (g x)

module Value = struct
  include Logic.Value

  let encode_without_tls ctx v w =
    let rec go = function
      | Colombe.State.Read {k; buffer; off; len} ->
        Colombe.State.Read {k= go <.> k; buffer; off; len}
      | Colombe.State.Write {k; buffer; off; len} ->
        Colombe.State.Write {k= go <.> k; buffer; off; len}
      | Colombe.State.Return v -> Return v
      | Colombe.State.Error err -> Error err in
    go (SSMTP.Value.encode ctx v w)

  let decode_without_tls ctx w =
    let rec go = function
      | Colombe.State.Read {k; buffer; off; len} ->
        Colombe.State.Read {k= go <.> k; buffer; off; len}
      | Colombe.State.Write {k; buffer; off; len} ->
        Colombe.State.Write {k= go <.> k; buffer; off; len}
      | Colombe.State.Return v -> Return v
      | Colombe.State.Error err -> Error err in
    go (SSMTP.Value.decode ctx w)
end

module Value_with_tls = Sendmail_with_starttls.Make_with_tls (Value)

module Monad
  : Logic.MONAD
    with type context = Sendmail_with_starttls.Context_with_tls.t
     and type error = Value_with_tls.error
= struct
  type context = Sendmail_with_starttls.Context_with_tls.t

  include State.Scheduler (Sendmail_with_starttls.Context_with_tls) (Value_with_tls)
end

type context = Sendmail_with_starttls.Context_with_tls.t

type error =
  [ `No_recipients
  | `Protocol of Value_with_tls.error
  | `Too_many_bad_commands
  | `Too_many_recipients
  | `Tls of Value_with_tls.error ]

let pp_value_with_tls_error ppf = function
  | `Tls_alert alert ->
    Fmt.pf ppf "TLS alert: %s" (Tls.Packet.alert_type_to_string alert)
  | `Tls_failure failure ->
    Fmt.pf ppf "TLS failure: %s" (Tls.Engine.string_of_failure failure)
  | `Tls_closed ->
    Fmt.string ppf "TLS connection closed by peer"
  | `Value (#Value.error as err) -> Value.pp_error ppf err

let pp_error ppf = function
  | `Tls (#Value_with_tls.error as err)
  | `Protocol (#Value_with_tls.error as err) ->
      pp_value_with_tls_error ppf err
  | `No_recipients -> Fmt.string ppf "No recipients"
  | `Too_many_bad_commands -> Fmt.string ppf "Too many bad commands"
  | `Too_many_recipients -> Fmt.string ppf "Too many recipients"

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

include Logic.Make (Monad)

let m_relay_init ctx info =
  match info.tls with
  | None ->
    let open Monad in
    send ctx Value.PP_220 [Colombe.Domain.to_string info.Ptt_common.domain]
    >>= fun () -> m_relay_init ctx info
  | Some tls ->
    let open Monad in
    let* _from_domain =
      send ctx Value.PP_220 [Colombe.Domain.to_string info.Ptt_common.domain]
      >>= fun () -> recv ctx Value.Helo in
    let capabilities =
      [
        politely ~domain:info.Ptt_common.domain ~ipaddr:info.Ptt_common.ipaddr; "8BITMIME"
      ; "SMTPUTF8"; "STARTTLS"; Fmt.str "SIZE %Ld" info.Ptt_common.size
      ] in
    let* () = send ctx Value.PP_250 capabilities in
    let reset = ref 0 and bad = ref 0 in
    let rec go () =
      if !reset >= 25 && !bad >= 25 then
        m_properly_close_and_fail ctx ~message:"You reached the limit buddy!"
          `Too_many_bad_commands
      else
        let* command = recv ctx Value.Any in
        match command with
        | `Verb ("STARTTLS", []) ->
          let* () = send ctx Value.PP_220 ["Go ahead buddy!"] in
          let decoder = Sendmail_with_starttls.Context_with_tls.decoder ctx in
          let tls_error err = `Tls err in
          Value_with_tls.starttls_as_server decoder tls
          |> reword_error tls_error
          >>= fun () -> m_relay_init ctx info
        | `Reset ->
          incr reset;
          let* () = send ctx Value.PP_250 ["Yes buddy!"] in
          go ()
        | `Quit -> m_politely_close ctx
        | `Hello _from_domain ->
            (* NOTE(dinosaure): [nstools.fr] asks [EHLO]/[HELO] two times. We must
               handle it correctly. *)
            incr bad;
            let* () = send ctx Value.PP_250 capabilities in
            go ()
        | _ ->
          incr bad;
          let* () =
            send ctx Value.PN_530 ["Must issue a STARTTLS command first."] in
          go () in
    go ()

let m_submission_init ctx info ms =
  match info.tls with
  | None ->
    let open Monad in
    send ctx Value.PP_220 [Colombe.Domain.to_string info.Ptt_common.domain]
    >>= fun () -> m_submission_init ctx info ms
  | Some tls ->
    let open Monad in
    let* _from_domain =
      send ctx Value.PP_220 [Colombe.Domain.to_string info.Ptt_common.domain]
      >>= fun () -> recv ctx Value.Helo in
    let capabilities =
      [
        politely ~domain:info.Ptt_common.domain ~ipaddr:info.Ptt_common.ipaddr; "8BITMIME"
      ; "SMTPUTF8"; "STARTTLS"
      ; Fmt.str "AUTH %a" Fmt.(list ~sep:(const string " ") Mechanism.pp) ms
      ; Fmt.str "SIZE %Ld" info.Ptt_common.size
      ] in
    let* () = send ctx Value.PP_250 capabilities in
    let reset = ref 0 and bad = ref 0 in
    let rec go () =
      if !reset >= 25 || !bad >= 25 then
        m_properly_close_and_fail ctx ~message:"You reached the limit buddy!"
          `Too_many_bad_commands
      else
        let* command = recv ctx Value.Any in
        match command with
        | `Verb ("STARTTLS", []) ->
          let* () = send ctx Value.PP_220 ["Go ahead buddy!"] in
          let decoder = Sendmail_with_starttls.Context_with_tls.decoder ctx in
          let tls_error err = `Tls err in
          Value_with_tls.starttls_as_server decoder tls
          |> reword_error tls_error
          >>= fun () -> m_submission_init ctx info ms
        | `Reset ->
          incr reset;
          let* () = send ctx Value.PP_250 ["Yes, buddy!"] in
          go ()
        | `Quit -> m_politely_close ctx
        | _ ->
          incr bad;
          let* () =
            send ctx Value.PN_530 ["Must issue a STARTTLS command first."] in
          go () in
    go ()
