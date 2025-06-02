let () = Printexc.record_backtrace true
let () = Fmt.set_utf_8 Fmt.stdout true
let () = Fmt.set_utf_8 Fmt.stderr true
let () = Fmt.set_style_renderer Fmt.stdout `Ansi_tty
let () = Fmt.set_style_renderer Fmt.stderr `Ansi_tty
let () = Logs.set_level ~all:true (Some Logs.Debug)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  {Logs.report}

let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Mirage_crypto_rng_unix.use_default ()
let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

module Scheduler = Colombe.Sigs.Make (struct type +'a t = 'a Lwt.t end)

let lwt =
  let open Lwt.Infix in
  let open Scheduler in
  {
    Colombe.Sigs.bind= (fun x f -> inj (prj x >>= fun x -> prj (f x)))
  ; Colombe.Sigs.return= (fun x -> inj (Lwt.return x))
  }

let ( <.> ) f g x = f (g x)
let rev f a b = f b a
let mechanism = Alcotest.testable Ptt.Mechanism.pp Ptt.Mechanism.equal

let msg =
  Alcotest.testable Rresult.R.pp_msg (fun (`Msg a) (`Msg b) -> String.equal a b)

let mechanism_test_0 =
  Alcotest_lwt.test_case "mechanism 0" `Quick @@ fun _sw () ->
  Alcotest.(check mechanism)
    "plain"
    (Ptt.Mechanism.of_string_exn "plain")
    Ptt.Mechanism.PLAIN;
  Alcotest.(check mechanism)
    "PLAIN"
    (Ptt.Mechanism.of_string_exn "PLAIN")
    Ptt.Mechanism.PLAIN;
  Alcotest.(check mechanism)
    "PlAiN"
    (Ptt.Mechanism.of_string_exn "PlAiN")
    Ptt.Mechanism.PLAIN;
  Alcotest.check_raises "PLAIZ" (Invalid_argument "Invalid mechanism: PLAIZ")
    (fun () -> ignore @@ Ptt.Mechanism.of_string_exn "PLAIZ");
  Lwt.return_unit

let auth0 =
  let module Map = Map.Make (struct
    type t = Emile.local

    let compare = Emile.compare_local ~case_sensitive:false
  end) in
  let open Mrmime.Mailbox in
  let m =
    Map.empty
    |> Map.add
         Local.(v [w "romain"; w "calascibetta"])
         Digestif.(digest_string SHA1 "toto")
    |> Map.add Local.(v [w "thomas"]) Digestif.(digest_string SHA1 "tata")
    |> Map.add Local.(v [w "anil"]) Digestif.(digest_string SHA1 "tutu")
    |> Map.add Local.(v [w "hannes"]) Digestif.(digest_string SHA1 "titi")
    |> Map.add Local.(v [w "gemma"]) Digestif.(digest_string SHA1 "") in
  let f username password =
    let username =
      match username with
      | `Dot_string vs -> List.map (fun x -> `Atom x) vs
      | `String _ -> assert false in
    match Map.find username m with
    | v -> Lwt.return Digestif.(equal SHA1 password v)
    | exception Not_found -> Lwt.return false in
  Ptt.Authentication.v f

let authentication_test_0 =
  Alcotest_lwt.test_case "authentication 0" `Quick @@ fun _sw () ->
  let open Lwt.Infix in
  let auth hash mechanism authenticator fmt =
    Fmt.kstr
      (fun payload ->
        Ptt.Authentication.decode_authentication hash mechanism authenticator
          (Base64.encode_exn payload))
      fmt in
  let plain_none = Ptt.Authentication.PLAIN None in
  auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "romain.calascibetta"
    "toto"
  >|= Result.map snd
  >>= fun romain ->
  Alcotest.(check (result bool msg)) "romain" (Ok true) romain;
  auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "thomas" "tata"
  >|= Result.map snd
  >>= fun thomas ->
  Alcotest.(check (result bool msg)) "thomas" (Ok true) thomas;
  auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "anil" "tutu"
  >|= Result.map snd
  >>= fun anil ->
  Alcotest.(check (result bool msg)) "anil" (Ok true) anil;
  auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "hannes" "titi"
  >|= Result.map snd
  >>= fun hannes ->
  Alcotest.(check (result bool msg)) "hannes" (Ok true) hannes;
  auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "gemma" ""
  >|= Result.map snd
  >>= fun gemma ->
  Alcotest.(check (result bool msg)) "gemma" (Ok true) gemma;
  auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "romain.calascibetta"
    "titi"
  >|= Result.map snd
  >>= fun wrong ->
  Alcotest.(check (result bool msg)) "romain (wrong)" (Ok false) wrong;
  auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "pierre.caillou" "toto"
  >|= Result.map snd
  >>= fun pierre ->
  Alcotest.(check (result bool msg)) "pierre" (Ok false) pierre;
  auth Digestif.SHA1 plain_none auth0 "stamp\000%s\000%s" "romain.calascibetta"
    "toto"
  >|= Result.map snd
  >>= fun bad_stamp ->
  Alcotest.(check (result bool msg))
    "bad stamp"
    (Error (`Msg "Unexpected stamp"))
    bad_stamp;
  auth Digestif.SHA1 plain_none auth0 "salut les copains" >|= Result.map snd
  >>= fun malformed ->
  Alcotest.(check (result bool msg))
    "malformed"
    (Error (`Msg "Invalid input"))
    malformed;
  auth Digestif.SHA1 (Ptt.Authentication.PLAIN (Some "stamp")) auth0
    "\000%s\000%s" "anil" "tutu"
  >|= Result.map snd
  >>= fun invalid_stamp ->
  Alcotest.(check (result bool msg))
    "no stamp"
    (Error (`Msg "Invalid stamp"))
    invalid_stamp;
  auth Digestif.SHA1 plain_none auth0 "\000\000%s" "tutu" >|= Result.map snd
  >>= fun invalid_username ->
  Alcotest.(check (result bool msg))
    "invalid username"
    (Error (`Msg "Invalid username: \"\""))
    invalid_username;
  Lwt.return_unit

let x25519 = Colombe.Domain.(Domain ["x25519"; "net"])
let gmail = Colombe.Domain.(Domain ["gmail"; "com"])
let recoil = Domain_name.(host_exn <.> of_string_exn) "recoil.org"
let nqsb = Domain_name.(host_exn <.> of_string_exn) "nqsb.io"
let gazagnaire = Domain_name.(host_exn <.> of_string_exn) "gazagnaire.org"
let put_crlf x = x ^ "\r\n"

let rdwr_from_flows inputs outputs =
  let inputs = ref (List.map put_crlf inputs) in
  let outputs = ref (List.map put_crlf outputs) in
  let open Scheduler in
  let rd () bytes off len =
    match !inputs with
    | [] -> inj (Lwt.return `End)
    | x :: r ->
      let len = min (String.length x) len in
      Bytes.blit_string x 0 bytes off len;
      if len = String.length x then inputs := r
      else inputs := String.sub x len (String.length x - len) :: r;
      inj (Lwt.return (`Len len)) in
  let rec wr () bytes off len =
    match !outputs with
    | [] -> Fmt.failwith "Unexpected output: %S" (String.sub bytes off len)
    | x :: r ->
      let max = len in
      let len = min (String.length x) len in
      if String.sub x 0 len <> String.sub bytes off len then
        Fmt.failwith "Expected %S, have %S" (String.sub x 0 len)
          (String.sub bytes off len);
      if String.length x = len then outputs := r
      else outputs := String.sub x len (String.length x - len) :: r;
      if len < max then wr () bytes (off + len) (max - len)
      else inj (Lwt.return ()) in
  ( {Colombe.Sigs.rd; Colombe.Sigs.wr}
  , fun () ->
      match !inputs, !outputs with
      | [], [] -> ()
      | r, w ->
        Fmt.failwith
          "inputs or outputs are not empty: @[<hov>%a@] and @[<hov>%a@]"
          Fmt.(Dump.list string)
          r
          Fmt.(Dump.list string)
          w )

let run_state m rdwr =
  let open Lwt.Infix in
  let open Scheduler in
  let rec go = function
    | Colombe.State.Write {buffer; off; len; k} ->
      rdwr.Colombe.Sigs.wr () buffer off len |> prj >>= fun () -> go (k len)
    | Colombe.State.Return v -> Lwt.return (Ok v)
    | Colombe.State.Error err -> Lwt.return (Error (`Error err))
    | Colombe.State.Read {buffer; off; len; k} ->
      rdwr.Colombe.Sigs.rd () buffer off len |> prj >>= fun res -> go (k res)
  in
  go m

let smtp_test_0 =
  Alcotest_lwt.test_case "SMTP (relay) 0" `Quick @@ fun _sw () ->
  let rdwr, check = rdwr_from_flows [] ["220 x25519.net"] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain= x25519
    ; ipaddr= Ipaddr.(V4 V4.localhost)
    ; tls= None
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 0L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_relay_init ctx info) rdwr >>= function
  | Ok _ -> Alcotest.fail "Unexpected good result"
  | Error (`Error (`Protocol `End_of_input)) ->
    Alcotest.(check unit) "empty stream" (check ()) ();
    Alcotest.(check pass) "connection close" () ();
    Lwt.return_unit
  | Error (`Error err) ->
    Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err

let smtp_test_1 =
  Alcotest_lwt.test_case "SMTP (relay) 1" `Quick @@ fun _sw () ->
  let rdwr, check =
    rdwr_from_flows ["EHLO gmail.com"; "QUIT"]
      [
        "220 x25519.net"; "250-x25519.net at your service, [127.0.0.1]"
      ; "250-8BITMIME"; "250-SMTPUTF8"; "250 SIZE 16777216"; "221 Bye, buddy!"
      ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain= x25519
    ; ipaddr= Ipaddr.(V4 V4.localhost)
    ; tls= None
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 16777216L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_relay_init ctx info) rdwr >>= function
  | Ok `Quit ->
    Alcotest.(check unit) "empty stream" (check ()) ();
    Alcotest.(check pass) "quit" () ();
    Lwt.return_unit
  | Ok (`Send _) -> Alcotest.fail "Unexpected submission"
  | Error (`Error err) ->
    Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.fail "Unexpected connection close"

let smtp_test_2 =
  Alcotest_lwt.test_case "SMTP (relay) 2" `Quick @@ fun _sw () ->
  let rdwr, check =
    rdwr_from_flows
      ["EHLO gmail.com"; "RSET"; "QUIT"]
      [
        "220 x25519.net"; "250-x25519.net at your service, [127.0.0.1]"
      ; "250-8BITMIME"; "250-SMTPUTF8"; "250 SIZE 16777216"; "250 Yes buddy!"
      ; "221 Bye, buddy!"
      ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain= x25519
    ; ipaddr= Ipaddr.(V4 V4.localhost)
    ; tls= None
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 16777216L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_relay_init ctx info) rdwr >>= function
  | Ok `Quit ->
    Alcotest.(check unit) "empty stream" (check ()) ();
    Alcotest.(check pass) "quit" () ();
    Lwt.return_unit
  | Ok (`Send _) -> Alcotest.fail "Unexpected submission"
  | Error (`Error err) ->
    Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.fail "Unexpected connection close"

let smtp_test_3 =
  Alcotest_lwt.test_case "SMTP (relay) 3" `Quick @@ fun _sw () ->
  let rdwr, check =
    rdwr_from_flows
      [
        "EHLO gmail.com"; "RSET"; "RSET"; "RSET"; "RSET"; "RSET"; "RSET"; "RSET"
      ; "RSET"; "RSET"; "RSET"; "RSET"; "RSET"; "RSET"; "RSET"; "RSET"; "RSET"
      ; "RSET"; "RSET"; "RSET"; "RSET"; "RSET"; "RSET"; "RSET"; "RSET"; "RSET"
      ]
      [
        "220 x25519.net"; "250-x25519.net at your service, [127.0.0.1]"
      ; "250-8BITMIME"; "250-SMTPUTF8"; "250 SIZE 16777216"; "250 Yes buddy!"
      ; "250 Yes buddy!"; "250 Yes buddy!"; "250 Yes buddy!"; "250 Yes buddy!"
      ; "250 Yes buddy!"; "250 Yes buddy!"; "250 Yes buddy!"; "250 Yes buddy!"
      ; "250 Yes buddy!"; "250 Yes buddy!"; "250 Yes buddy!"; "250 Yes buddy!"
      ; "250 Yes buddy!"; "250 Yes buddy!"; "250 Yes buddy!"; "250 Yes buddy!"
      ; "250 Yes buddy!"; "250 Yes buddy!"; "250 Yes buddy!"; "250 Yes buddy!"
      ; "250 Yes buddy!"; "250 Yes buddy!"; "250 Yes buddy!"; "250 Yes buddy!"
      ; "554 You reached the limit buddy!"
      ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain= x25519
    ; ipaddr= Ipaddr.(V4 V4.localhost)
    ; tls= None
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 16777216L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_relay_init ctx info) rdwr >>= function
  | Ok (`Quit | `Send _) -> Alcotest.fail "Unexpected quit or submission"
  | Error (`Error `Too_many_bad_commands) ->
    Alcotest.(check unit) "empty stream" (check ()) ();
    Alcotest.(check pass) "too many bad commands" () ();
    Lwt.return_unit
  | Error (`Error err) ->
    Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.fail "Unexpected connection close"

let smtp_test_4 =
  Alcotest_lwt.test_case "SMTP (relay) 4" `Quick @@ fun _sw () ->
  let rdwr, check =
    rdwr_from_flows
      ["EHLO gmail.com"; "MAIL FROM:<romain.calascibetta@gmail.com>"; "DATA"]
      [
        "220 x25519.net"; "250-x25519.net at your service, [127.0.0.1]"
      ; "250-8BITMIME"; "250-SMTPUTF8"; "250 SIZE 16777216"; "250 Ok, buddy!"
      ; "554 No recipients"
      ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain= x25519
    ; ipaddr= Ipaddr.(V4 V4.localhost)
    ; tls= None
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 16777216L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_relay_init ctx info) rdwr >>= function
  | Ok _ -> Alcotest.fail "Unexpected quit or submission"
  | Error (`Error `No_recipients) ->
    Alcotest.(check unit) "empty stream" (check ()) ();
    Alcotest.(check pass) "no recipients" () ();
    Lwt.return_unit
  | Error (`Error err) ->
    Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.fail "Unexpected connection close"

let reverse_path =
  Alcotest.testable Colombe.Reverse_path.pp Colombe.Reverse_path.equal

let forward_path =
  Alcotest.testable Colombe.Forward_path.pp Colombe.Forward_path.equal

let domain = Alcotest.testable Colombe.Domain.pp Colombe.Domain.equal

let smtp_test_5 =
  Alcotest_lwt.test_case "SMTP (relay) 5" `Quick @@ fun _sw () ->
  let rdwr, check =
    rdwr_from_flows
      [
        "EHLO gmail.com"; "MAIL FROM:<romain.calascibetta@gmail.com>"
      ; "RCPT TO:<anil@recoil.org>"; "DATA"
      ]
      [
        "220 x25519.net"; "250-x25519.net at your service, [127.0.0.1]"
      ; "250-8BITMIME"; "250-SMTPUTF8"; "250 SIZE 16777216"; "250 Ok, buddy!"
      ; "250 Ok, buddy!"
      ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain= x25519
    ; ipaddr= Ipaddr.(V4 V4.localhost)
    ; tls= None
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 16777216L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_relay_init ctx info) rdwr >>= function
  | Ok (`Send {Ptt.SSMTP.from; Ptt.SSMTP.recipients; Ptt.SSMTP.domain_from}) ->
    let romain_calascibetta =
      let open Mrmime.Mailbox in
      Local.[w "romain"; w "calascibetta"]
      @ Domain.(domain, [a "gmail"; a "com"]) in
    let anil =
      let open Mrmime.Mailbox in
      Local.[w "anil"] @ Domain.(domain, [a "recoil"; a "org"]) in
    let gmail =
      let open Mrmime.Mailbox in
      Domain.(v domain [a "gmail"; a "com"]) in
    Alcotest.(check reverse_path)
      "from" (fst from)
      ((Rresult.R.get_ok <.> Colombe_emile.to_reverse_path) romain_calascibetta);
    Alcotest.(check (list forward_path))
      "recipients" (List.map fst recipients)
      [(Rresult.R.get_ok <.> Colombe_emile.to_forward_path) anil];
    Alcotest.(check domain)
      "domain" domain_from
      ((Rresult.R.get_ok <.> Colombe_emile.to_domain) gmail);
    Alcotest.(check unit) "empty stream" (check ()) ();
    Alcotest.(check pass) "submission" () ();
    Lwt.return_unit
  | Ok `Quit -> Alcotest.fail "Unexpected quit"
  | Error (`Error err) ->
    Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.fail "Unexpected connection close"

let smtp_test_6 =
  Alcotest_lwt.test_case "SMTP (submission) 6" `Quick @@ fun _sw () ->
  let rdwr, check =
    rdwr_from_flows
      ["EHLO gmail.com"; "MAIL FROM:<romain.calascibetta@gmail.com>"; "QUIT"]
      [
        "220 x25519.net"; "250-x25519.net at your service, [127.0.0.1]"
      ; "250-8BITMIME"; "250-SMTPUTF8"; "250-SIZE 16777216"; "250 AUTH PLAIN"
      ; "530 Authentication required, buddy!"; "221 Bye, buddy!"
      ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain= x25519
    ; ipaddr= Ipaddr.(V4 V4.localhost)
    ; tls= None
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 16777216L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_submission_init ctx info [Ptt.Mechanism.PLAIN]) rdwr
  >>= function
  | Ok `Quit ->
    Alcotest.(check unit) "empty stream" (check ()) ();
    Alcotest.(check pass) "quit" () ();
    Lwt.return_unit
  | Ok (`Authentication _ | `Authentication_with_payload _) ->
    Alcotest.failf "Unexpected authentication"
  | Ok (`Submission _) -> Alcotest.failf "Unexpected submission"
  | Error (`Error err) ->
    Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.failf "Unexpected connection close"

let smtp_test_7 =
  Alcotest_lwt.test_case "SMTP (submission) 7" `Quick @@ fun _sw () ->
  let rdwr, check =
    rdwr_from_flows
      ["EHLO gmail.com"; "AUTH PLAIN"]
      [
        "220 x25519.net"; "250-x25519.net at your service, [127.0.0.1]"
      ; "250-8BITMIME"; "250-SMTPUTF8"; "250-SIZE 16777216"; "250 AUTH PLAIN"
      ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain= x25519
    ; ipaddr= Ipaddr.(V4 V4.localhost)
    ; tls= None
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 16777216L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_submission_init ctx info [Ptt.Mechanism.PLAIN]) rdwr
  >>= function
  | Ok (`Authentication (v, m) | `Authentication_with_payload (v, m, _)) ->
    let gmail =
      let open Mrmime.Mailbox in
      Domain.(v domain [a "gmail"; a "com"]) in
    Alcotest.(check unit) "empty stream" (check ()) ();
    Alcotest.(check mechanism) "mechanism" m Ptt.Mechanism.PLAIN;
    Alcotest.(check domain)
      "domain" v
      ((Rresult.R.get_ok <.> Colombe_emile.to_domain) gmail);
    Alcotest.(check pass) "authentication" () ();
    Lwt.return_unit
  | Ok `Quit | Ok (`Submission _) ->
    Alcotest.failf "Unexpected quit or submission"
  | Error (`Error err) ->
    Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.failf "Unexpected connection close"

module Server = Ptt_server.Make (Tcpip_stack_socket.V4V6)

let resolver =
  let open Ptt_common in
  let getmxbyname _ domain_name =
    let mxs =
      Dns.Rr_map.Mx_set.add
        {Dns.Mx.preference= 0; mail_exchange= domain_name}
        Dns.Rr_map.Mx_set.empty in
    Lwt.return (Ok mxs) in
  let gethostbyname tbl domain_name =
    match Hashtbl.find_opt tbl domain_name with
    | Some v -> Lwt.return (Ok v)
    | None ->
      let err = Rresult.R.error_msgf "%a not found" Domain_name.pp domain_name in
      Lwt.return err in
  {getmxbyname; gethostbyname}

let make_smtp_server ?stop ~port tbl info stack =
  let open Lwt.Infix in
  let module SMTP = Ptt.Relay.Make (Tcpip_stack_socket.V4V6) in
  let ic_server, stream0, close = SMTP.create ~info in
  let job_server server =
    let handler flow =
      let ipaddr, port = Tcpip_stack_socket.V4V6.TCP.dst flow in
      Lwt.finalize
        (fun () ->
          SMTP.accept ~ipaddr flow tbl resolver server
          >|= Result.map_error (Rresult.R.msgf "%a" SMTP.pp_error))
        (fun () -> Tcpip_stack_socket.V4V6.TCP.close flow)
      >>= function
      | Ok () -> Lwt.return ()
      | Error (`Msg err) ->
        Logs.err (fun m ->
            m "<%a:%d> raised an error: %s" Ipaddr.pp ipaddr port err);
        Lwt.return () in
    Server.init ~port stack >|= fun service ->
    Server.serve_when_ready ?stop ~handler service in
  job_server ic_server >|= fun (`Initialized th) ->
  let th = th >|= close in
  `Initialized th, stream0

module Happy_eyeballs_daemon =
  Happy_eyeballs_mirage.Make (Tcpip_stack_socket.V4V6)

module Sendmail =
  Sendmail_mirage.Make (Tcpip_stack_socket.V4V6.TCP) (Happy_eyeballs_daemon)

let sendmail he ipaddr port ~domain sender recipients contents =
  let open Lwt.Infix in
  let destination = `Ipaddrs [ipaddr] in
  let stream = Lwt_stream.of_list contents in
  let stream = Lwt_stream.map (fun str -> str ^ "\r\n") stream in
  let mail () =
    Lwt_stream.get stream >|= function
    | Some str -> Some (str, 0, String.length str)
    | None -> None in
  Sendmail.sendmail he ~destination ~port ~domain sender recipients mail
  >>= function
  | Ok () -> Lwt.return_unit
  | Error (`Msg msg) -> Fmt.failwith "%s" msg
  | Error (#Sendmail_with_starttls.error as err) ->
    Fmt.failwith "%a" Sendmail_with_starttls.pp_error err

let key = Alcotest.testable Ptt.Msgd.pp Ptt.Msgd.equal

let full_test_0 =
  Alcotest_lwt.test_case "Receive one email from Anil" `Quick @@ fun _sw () ->
  let romain_calascibetta =
    let open Mrmime.Mailbox in
    (Rresult.R.get_ok <.> Colombe_emile.to_forward_path)
      (Local.[w "romain"; w "calascibetta"]
      @ Domain.(domain, [a "gmail"; a "com"])) in
  let anil =
    let open Mrmime.Mailbox in
    (Rresult.R.get_ok <.> Colombe_emile.to_reverse_path)
      (Local.[w "anil"] @ Domain.(domain, [a "recoil"; a "org"])) in
  let recoil = (Colombe.Domain.of_string_exn <.> Domain_name.to_string) recoil in
  let ipv4_only = false and ipv6_only = false in
  let open Lwt.Infix in
  let open Tcpip_stack_socket.V4V6 in
  TCP.connect ~ipv4_only ~ipv6_only Ipaddr.V4.Prefix.global None
  >>= fun tcpv4v6 ->
  UDP.connect ~ipv4_only ~ipv6_only Ipaddr.V4.Prefix.global None
  >>= fun udpv4v6 ->
  connect udpv4v6 tcpv4v6 >>= fun stack ->
  let he = Happy_eyeballs_daemon.create stack in
  let sendmail contents =
    sendmail he
      Ipaddr.(V4 V4.localhost)
      8888 ~domain:recoil anil [romain_calascibetta] contents in
  let stop = Lwt_switch.create () in
  let open Lwt.Infix in
  let tbl = Hashtbl.create 0 in
  make_smtp_server ~stop ~port:8888 tbl
    {
      Ptt.SMTP.domain= gmail
    ; ipaddr= Ipaddr.(V4 V4.localhost)
    ; tls= None
    ; zone= Mrmime.Date.Zone.GMT
    ; size= 0x1000000L
    }
    tcpv4v6
  >>= fun (`Initialized th, stream) ->
  let sendmail =
    sendmail
      [
        "From: anil@recoil.org"; "Subject: SMTP server, PLZ!"; ""; "Hello World!"
      ]
    >>= fun () ->
    Lwt_switch.turn_off stop >|= fun () -> `Done in
  let fold (key, _, wk) acc =
    let acc = match acc with `Done -> [] | `Inbox acc -> acc in
    Lwt.wakeup_later wk `Ok;
    Lwt.return (`Inbox (key :: acc)) in
  Lwt.all
    [
      sendmail; (th >|= fun () -> `Done)
    ; Lwt_stream.fold_s fold stream (`Inbox [])
    ]
  >>= fun results ->
  let[@warning "-8"] [`Done; `Done; `Inbox inbox] = results in
  Alcotest.(check (list key))
    "inbox" inbox
    [
      Ptt.Msgd.key ~domain_from:recoil ~from:(anil, [])
        ~recipients:[romain_calascibetta, []]
        ~ipaddr:(Ipaddr.V4 Ipaddr.V4.localhost) 0L
    ];
  Lwt.return_unit

let full_test_1 =
  Alcotest_lwt.test_case "Receive emails from Anil and Thomas" `Quick
  @@ fun _sw () ->
  let romain_calascibetta =
    let open Mrmime.Mailbox in
    (Rresult.R.get_ok <.> Colombe_emile.to_forward_path)
      (Local.[w "romain"; w "calascibetta"]
      @ Domain.(domain, [a "gmail"; a "com"])) in
  let anil =
    let open Mrmime.Mailbox in
    (Rresult.R.get_ok <.> Colombe_emile.to_reverse_path)
      (Local.[w "anil"] @ Domain.(domain, [a "recoil"; a "org"])) in
  let thomas =
    let open Mrmime.Mailbox in
    (Rresult.R.get_ok <.> Colombe_emile.to_reverse_path)
      (Local.[w "thomas"] @ Domain.(domain, [a "gazagnaire"; a "org"])) in
  let recoil = (Colombe.Domain.of_string_exn <.> Domain_name.to_string) recoil in
  let gazagnaire =
    (Colombe.Domain.of_string_exn <.> Domain_name.to_string) gazagnaire in
  let ipv4_only = false and ipv6_only = false in
  let open Lwt.Infix in
  let open Tcpip_stack_socket.V4V6 in
  TCP.connect ~ipv4_only ~ipv6_only Ipaddr.V4.Prefix.global None
  >>= fun tcpv4v6 ->
  UDP.connect ~ipv4_only ~ipv6_only Ipaddr.V4.Prefix.global None
  >>= fun udpv4v6 ->
  connect udpv4v6 tcpv4v6 >>= fun stack ->
  let he = Happy_eyeballs_daemon.create stack in
  let stop = Lwt_switch.create () in
  let open Lwt.Infix in
  let tbl = Hashtbl.create 0 in
  make_smtp_server ~stop ~port:4444 tbl
    {
      Ptt.SMTP.domain= gmail
    ; ipaddr= Ipaddr.(V4 V4.localhost)
    ; tls= None
    ; zone= Mrmime.Date.Zone.GMT
    ; size= 0x1000000L
    }
    tcpv4v6
  >>= fun (`Initialized th, stream) ->
  let sendmail ~domain sender contents =
    sendmail he
      Ipaddr.(V4 V4.localhost)
      4444 ~domain sender [romain_calascibetta] contents in
  let sendmail =
    sendmail ~domain:recoil anil
      [
        "From: anil@recoil.org"; "Subject: SMTP server, PLZ!"; ""; "Hello World!"
      ]
    >>= fun () ->
    sendmail ~domain:gazagnaire thomas
      [
        "From: anil@recoil.org"; "Subject: SMTP server, PLZ!"; ""; "Hello World!"
      ]
    >>= fun () ->
    Lwt_switch.turn_off stop >|= fun () -> `Done in
  let fold (key, _, wk) acc =
    let acc = match acc with `Done -> [] | `Inbox acc -> acc in
    Lwt.wakeup_later wk `Ok;
    Lwt.return (`Inbox (key :: acc)) in
  Lwt.all
    [sendmail; (th >|= fun () -> `Done); Lwt_stream.fold_s fold stream `Done]
  >>= fun results ->
  let[@warning "-8"] [`Done; `Done; `Inbox inbox] = results in
  Alcotest.(check (list key))
    "inbox" inbox
    [
      Ptt.Msgd.key ~domain_from:gazagnaire ~from:(thomas, [])
        ~recipients:[romain_calascibetta, []]
        ~ipaddr:(Ipaddr.V4 Ipaddr.V4.localhost) 1L
    ; Ptt.Msgd.key ~domain_from:recoil ~from:(anil, [])
        ~recipients:[romain_calascibetta, []]
        ~ipaddr:(Ipaddr.V4 Ipaddr.V4.localhost) 0L
    ];
  Lwt.return_unit

let fiber =
  Alcotest_lwt.run "ptt"
    [
      "mechanism", [mechanism_test_0]; "authentication", [authentication_test_0]
    ; ( "SMTP"
      , [
          smtp_test_0; smtp_test_1; smtp_test_2; smtp_test_3; smtp_test_4
        ; smtp_test_5; smtp_test_6; smtp_test_7
        ] ); "server", [full_test_0; full_test_1]
    ]

let () = Lwt_main.run fiber
