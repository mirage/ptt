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
      | Colombe.State.Error err -> Error (`Protocol err) in
    go (SSMTP.Value.encode ctx v w)

  let decode_without_tls ctx w =
    let rec go = function
      | Colombe.State.Read {k; buffer; off; len} ->
        Colombe.State.Read {k= go <.> k; buffer; off; len}
      | Colombe.State.Write {k; buffer; off; len} ->
        Colombe.State.Write {k= go <.> k; buffer; off; len}
      | Colombe.State.Return v -> Return v
      | Colombe.State.Error err -> Error (`Protocol err) in
    go (SSMTP.Value.decode ctx w)
end

module Value_with_tls = Sendmail_with_starttls.Make_with_tls (Value)

module Monad = struct
  type context = Sendmail_with_starttls.Context_with_tls.t

  include
    State.Scheduler (Sendmail_with_starttls.Context_with_tls) (Value_with_tls)
end

type context = Sendmail_with_starttls.Context_with_tls.t

type error =
  [ `Protocol of
    [ `Protocol of Value.error
    | `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure
    | `Tls_closed ]
  | `Tls of
    [ `Protocol of Value.error
    | `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure
    | `Tls_closed ]
  | `No_recipients
  | `Invalid_recipients
  | `Too_many_bad_commands
  | `Too_many_recipients ]

let pp_error ppf = function
  | `Tls (`Protocol (#Value.error as err))
  | `Protocol (`Protocol (#Value.error as err)) ->
    Value.pp_error ppf err
  | `Protocol (`Tls_alert alert) | `Tls (`Tls_alert alert) ->
    Fmt.pf ppf "TLS alert: %s" (Tls.Packet.alert_type_to_string alert)
  | `Protocol (`Tls_failure failure) | `Tls (`Tls_failure failure) ->
    Fmt.pf ppf "TLS failure: %s" (Tls.Engine.string_of_failure failure)
  | `Tls `Tls_closed | `Protocol `Tls_closed ->
    Fmt.string ppf "TLS connection closed by peer"
  | `No_recipients -> Fmt.string ppf "No recipients"
  | `Invalid_recipients -> Fmt.string ppf "Invalid recipients"
  | `Too_many_bad_commands -> Fmt.string ppf "Too many bad commands"
  | `Too_many_recipients -> Fmt.string ppf "Too many recipients"

type info = Logic.info = {
    domain: [ `host ] Domain_name.t
  ; ipaddr: Ipaddr.t
  ; tls: Tls.Config.server option
  ; zone: Mrmime.Date.Zone.t
  ; size: int64
}

type submission = Logic.submission = {
    from: Messaged.from
  ; recipients: (Forward_path.t * (string * string option) list) list
  ; domain_from: Domain.t
}

include Logic.Make (Monad)

let m_relay_init ctx info =
  match info.tls with
  | None ->
    let open Monad in
    send ctx Value.PP_220 [Domain_name.to_string info.Logic.domain]
    >>= fun () -> m_relay_init ctx info
  | Some tls ->
    let open Monad in
    let* _from_domain =
      send ctx Value.PP_220 [Domain_name.to_string info.Logic.domain]
      >>= fun () -> recv ctx Value.Helo in
    let capabilities =
      [
        politely ~domain:info.Logic.domain ~ipaddr:info.Logic.ipaddr; "8BITMIME"
      ; "SMTPUTF8"; "STARTTLS"; Fmt.str "SIZE %Ld" info.Logic.size
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
          incr reset
          ; let* () = send ctx Value.PP_250 ["Yes buddy!"] in
            go ()
        | `Quit -> m_politely_close ctx
        | _ ->
          incr bad
          ; let* () =
              send ctx Value.PN_530 ["Must issue a STARTTLS command first."]
            in
            go () in
    go ()

let m_submission_init ctx info ms =
  match info.tls with
  | None ->
    let open Monad in
    send ctx Value.PP_220 [Domain_name.to_string info.Logic.domain]
    >>= fun () -> m_submission_init ctx info ms
  | Some tls ->
    let open Monad in
    let* _from_domain =
      send ctx Value.PP_220 [Domain_name.to_string info.Logic.domain]
      >>= fun () -> recv ctx Value.Helo in
    let capabilities =
      [
        politely ~domain:info.Logic.domain ~ipaddr:info.Logic.ipaddr; "8BITMIME"
      ; "SMTPUTF8"; "STARTTLS"
      ; Fmt.str "AUTH %a" Fmt.(list ~sep:(const string " ") Mechanism.pp) ms
      ; Fmt.str "SIZE %Ld" info.Logic.size
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
          incr reset
          ; let* () = send ctx Value.PP_250 ["Yes, buddy!"] in
            go ()
        | `Quit -> m_politely_close ctx
        | _ ->
          incr bad
          ; let* () =
              send ctx Value.PN_530 ["Must issue a STARTTLS command first."]
            in
            go () in
    go ()
