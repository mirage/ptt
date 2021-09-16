let () = Printexc.record_backtrace true
let reporter = Logs_fmt.reporter ()
let () = Fmt.set_utf_8 Fmt.stdout true
let () = Fmt.set_utf_8 Fmt.stderr true
let () = Fmt.set_style_renderer Fmt.stdout `Ansi_tty
let () = Fmt.set_style_renderer Fmt.stderr `Ansi_tty
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Logs.set_reporter reporter
let () = Mirage_crypto_rng_unix.initialize ()
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
    Ptt.Mechanism.PLAIN
  ; Alcotest.(check mechanism)
      "PLAIN"
      (Ptt.Mechanism.of_string_exn "PLAIN")
      Ptt.Mechanism.PLAIN
  ; Alcotest.(check mechanism)
      "PlAiN"
      (Ptt.Mechanism.of_string_exn "PlAiN")
      Ptt.Mechanism.PLAIN
  ; Alcotest.check_raises "PLAIZ" (Invalid_argument "Invalid mechanism: PLAIZ")
      (fun () -> ignore @@ Ptt.Mechanism.of_string_exn "PLAIZ")
  ; Lwt.return_unit

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
    match Map.find username m with
    | v -> Scheduler.inj (Lwt.return Digestif.(equal SHA1 password v))
    | exception Not_found -> Scheduler.inj (Lwt.return false) in
  Ptt.Authentication.v f

let authentication_test_0 =
  Alcotest_lwt.test_case "authentication 0" `Quick @@ fun _sw () ->
  let auth hash mechanism authenticator fmt =
    Fmt.kstrf
      (fun payload ->
        Ptt.Authentication.decode_authentication lwt hash mechanism
          authenticator
          (Base64.encode_exn payload)
        |> Scheduler.prj)
      fmt in
  let plain_none = Ptt.Authentication.PLAIN None in
  let open Lwt.Infix in
  auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "romain.calascibetta"
    "toto"
  >>= fun romain ->
  Alcotest.(check (result bool msg)) "romain" (Ok true) romain
  ; auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "thomas" "tata"
    >>= fun thomas ->
    Alcotest.(check (result bool msg)) "thomas" (Ok true) thomas
    ; auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "anil" "tutu"
      >>= fun anil ->
      Alcotest.(check (result bool msg)) "anil" (Ok true) anil
      ; auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "hannes" "titi"
        >>= fun hannes ->
        Alcotest.(check (result bool msg)) "hannes" (Ok true) hannes
        ; auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "gemma" ""
          >>= fun gemma ->
          Alcotest.(check (result bool msg)) "gemma" (Ok true) gemma
          ; auth Digestif.SHA1 plain_none auth0 "\000%s\000%s"
              "romain.calascibetta" "titi"
            >>= fun wrong ->
            Alcotest.(check (result bool msg)) "romain (wrong)" (Ok false) wrong
            ; auth Digestif.SHA1 plain_none auth0 "\000%s\000%s"
                "pierre.caillou" "toto"
              >>= fun pierre ->
              Alcotest.(check (result bool msg)) "pierre" (Ok false) pierre
              ; auth Digestif.SHA1 plain_none auth0 "stamp\000%s\000%s"
                  "romain.calascibetta" "toto"
                >>= fun bad_stamp ->
                Alcotest.(check (result bool msg))
                  "bad stamp"
                  (Error (`Msg "Unexpected stamp"))
                  bad_stamp
                ; auth Digestif.SHA1 plain_none auth0 "salut les copains"
                  >>= fun malformed ->
                  Alcotest.(check (result bool msg))
                    "malformed"
                    (Error (`Msg "Invalid input"))
                    malformed
                  ; auth Digestif.SHA1 (Ptt.Authentication.PLAIN (Some "stamp"))
                      auth0 "\000%s\000%s" "anil" "tutu"
                    >>= fun invalid_stamp ->
                    Alcotest.(check (result bool msg))
                      "no stamp"
                      (Error (`Msg "Invalid stamp (\"stamp\" <> \"\")"))
                      invalid_stamp
                    ; auth Digestif.SHA1 plain_none auth0 "\000\000%s" "tutu"
                      >>= fun invalid_username ->
                      Alcotest.(check (result bool msg))
                        "invalid username"
                        (Error (`Msg "Invalid username: \"\""))
                        invalid_username
                      ; Lwt.return_unit

let x25519 = Domain_name.(host_exn <.> of_string_exn) "x25519.net"
let gmail = Domain_name.(host_exn <.> of_string_exn) "gmail.com"
let recoil = Domain_name.(host_exn <.> of_string_exn) "recoil.org"
let nqsb = Domain_name.(host_exn <.> of_string_exn) "nqsb.io"
let gazagnaire = Domain_name.(host_exn <.> of_string_exn) "gazagnaire.org"

let pp_unresolved ppf = function
  | `All -> Fmt.string ppf "<all>"
  | `Postmaster -> Fmt.string ppf "<postmaster>"
  | `Local vs -> Fmt.Dump.list Emile.pp_local ppf vs

let equal_unresolved a b =
  match a, b with
  | `All, `All -> true
  | `Postmaster, `Postmaster -> true
  | `Local a, `Local b -> (
    let a = List.sort (Emile.compare_local ~case_sensitive:true) a in
    let b = List.sort (Emile.compare_local ~case_sensitive:true) b in
    try List.for_all2 (Emile.equal_local ~case_sensitive:true) a b
    with _ -> false)
  | _, _ -> false

let unresolved = Alcotest.testable pp_unresolved equal_unresolved

let aggregate_test_0 =
  Alcotest_lwt.test_case "aggregate 0" `Quick @@ fun _sw () ->
  let open Mrmime.Mailbox in
  let m0 =
    Local.[w "romain"; w "calascibetta"] @ Domain.(domain, [a "gmail"; a "com"])
  in
  let m1 = Local.[w "thomas"] @ Domain.(domain, [a "gazagnaire"; a "org"]) in
  let m2 = Local.[w "anil"] @ Domain.(domain, [a "recoil"; a "org"]) in
  let m3 =
    Local.[w "gemma"; w "d"; w "gordon"] @ Domain.(domain, [a "gmail"; a "com"])
  in
  let ms =
    List.map
      (Rresult.R.get_ok <.> Colombe_emile.to_forward_path)
      [m0; m1; m2; m3] in
  let ms =
    Colombe.Forward_path.Domain
      ((Rresult.R.get_ok <.> Colombe_emile.to_domain)
         Domain.(v domain [a "nqsb"; a "io"]))
    :: ms in
  let u, r = Ptt.Aggregate.aggregate_by_domains ~domain:x25519 ms in
  Alcotest.(check bool)
    "resolved is empty" true
    (Ptt.Aggregate.By_ipaddr.is_empty r)
  ; Alcotest.(check unresolved)
      "unresolved gmail.com"
      (`Local
        [
          Local.(v [w "romain"; w "calascibetta"])
        ; Local.(v [w "gemma"; w "d"; w "gordon"])
        ])
      (Ptt.Aggregate.By_domain.find gmail u)
  ; Alcotest.(check unresolved)
      "unresolved recoil.org"
      (`Local [Local.(v [w "anil"])])
      (Ptt.Aggregate.By_domain.find recoil u)
  ; Alcotest.(check unresolved)
      "unresolved gazagnaire.org"
      (`Local [Local.(v [w "thomas"])])
      (Ptt.Aggregate.By_domain.find gazagnaire u)
  ; Alcotest.(check unresolved)
      "unresolved nqsb.io" `All
      (Ptt.Aggregate.By_domain.find nqsb u)
  ; Lwt.return_unit

module Lwt_io = struct
  include Lwt

  module Condition = struct
    type +'a fiber = 'a Lwt.t
    type mutex = Lwt_mutex.t
    type t = unit Lwt_condition.t

    let wait (t : t) mutex = Lwt_condition.wait ~mutex t
    let signal t = Lwt_condition.signal t ()
    let broadcast t = Lwt_condition.broadcast t ()
    let create () = Lwt_condition.create ()
  end

  module Mutex = struct
    type +'a fiber = 'a Lwt.t

    include Lwt_mutex
  end
end

module Md = Ptt.Messaged.Make (Scheduler) (Lwt_io)

let stream_of_string_list l =
  let l = ref l in
  let stream () =
    match !l with
    | [] -> Lwt.return None
    | x :: r ->
      l := r
      ; Lwt.return (Some x) in
  stream

let stream_is_empty s =
  let open Lwt.Infix in
  s () >>= function Some _ -> Lwt.return false | None -> Lwt.return true

let hello_world () =
  stream_of_string_list ["Hello", 0, 5; " ", 0, 1; "World!", 0, 6]

let hello_buddy () =
  stream_of_string_list ["Hello", 0, 5; " ", 0, 1; "buddy!", 0, 6]

let hello_guy () =
  stream_of_string_list ["Hello", 0, 5; " ", 0, 1; "guy!", 0, 4]

let messaged_test_0 =
  Alcotest_lwt.test_case "messaged 0" `Quick @@ fun _sw () ->
  let md = Md.create () in
  let do0 ~domain_from ~from v =
    let open Lwt.Infix in
    let key =
      Ptt.Messaged.v
        ~domain_from:
          ((Rresult.R.get_ok <.> Colombe_emile.to_domain) domain_from)
        ~from:((Rresult.R.get_ok <.> Colombe_emile.to_reverse_path) from, [])
        ~recipients:[] 0L in
    Md.push md key >>= fun producer ->
    let v = v () (* XXX(dinosaure): really create the stream. *) in
    let rec consume () =
      v () >>= function
      | Some chunk -> producer (Some chunk) >>= consume
      | None -> producer None in
    consume () in
  let contents = ref "" in
  let do1 () =
    let open Lwt.Infix in
    Md.await md >>= fun () ->
    Md.pop md >>= function
    | Some (_, _, v) ->
      let buf = Buffer.create 0x100 in
      let rec consume () =
        v () >>= function
        | Some (str, off, len) ->
          Buffer.add_substring buf str off len
          ; consume ()
        | None ->
          contents := Buffer.contents buf
          ; Lwt.return_unit in
      consume ()
    | None -> assert false in
  let domain_from = Mrmime.Mailbox.Domain.(v domain [a "x25519"; a "net"]) in
  let from =
    let open Mrmime.Mailbox in
    Local.[w "romain"; w "calascibetta"] @ Domain.(domain, [a "gmail"; a "com"])
  in

  let open Lwt.Infix in
  Lwt.both (do0 ~domain_from ~from hello_world) (do1 ()) >>= fun _ ->
  Alcotest.(check string) "(random schedule) payload" !contents "Hello World!"
  ; Lwt.both (do1 ()) (do0 ~domain_from ~from hello_buddy) >>= fun _ ->
    Alcotest.(check string)
      "(consumer & producer) payload" !contents "Hello buddy!"
    ; Lwt.both (do0 ~domain_from ~from hello_guy) (do1 ()) >>= fun _ ->
      Alcotest.(check string)
        "(producer & consumer) payload" !contents "Hello guy!"
      ; Lwt.return_unit

let messaged_test_1 =
  Alcotest_lwt.test_case "messaged 1" `Quick @@ fun _sw () ->
  let md = Md.create () in
  let last = ref 0 in
  let do0 ~domain_from ~from v =
    let open Lwt.Infix in
    last := 0
    ; let key =
        Ptt.Messaged.v
          ~domain_from:
            ((Rresult.R.get_ok <.> Colombe_emile.to_domain) domain_from)
          ~from:((Rresult.R.get_ok <.> Colombe_emile.to_reverse_path) from, [])
          ~recipients:[] 0L in
      Md.push md key >>= fun producer ->
      let rec consume () =
        v () >>= function
        | Some chunk -> producer (Some chunk) >>= fun () -> consume ()
        | None -> producer None in
      consume () in
  let do1 () =
    let open Lwt.Infix in
    last := 1
    ; Md.await md >>= fun () ->
      (* XXX(dinosaure): schedule [do1] __after__ [do0]. *)
      Md.pop md >>= function
      | Some (_, q, _) -> Md.close q (* XXX(dinosaure): unlock [do0]. *)
      | None -> assert false in
  let domain_from = Mrmime.Mailbox.Domain.(v domain [a "x25519"; a "net"]) in
  let from =
    let open Mrmime.Mailbox in
    Local.[w "romain"; w "calascibetta"] @ Domain.(domain, [a "gmail"; a "com"])
  in
  let open Lwt.Infix in
  let stream = hello_world () in
  Lwt.both (do0 ~domain_from ~from stream) (do1 ()) >>= fun _ ->
  stream_is_empty stream >>= fun res0 ->
  Alcotest.(check bool) "stream consumed" res0 true
  ; Alcotest.(check pass) "random schedule" () ()
  ; let stream = hello_buddy () in
    Lwt.both (do1 ())
      (Lwt_unix.sleep 0.5 >>= fun () -> do0 ~domain_from ~from stream)
    >>= fun _ ->
    stream_is_empty stream >>= fun res1 ->
    Alcotest.(check bool) "stream consumed" res1 true
    ; Alcotest.(check int) "(consumer & producer)" !last 0
    ; let stream = hello_guy () in
      Lwt.both (do0 ~domain_from ~from stream) (Lwt_unix.sleep 0.5 >>= do1)
      >>= fun _ ->
      stream_is_empty stream >>= fun res2 ->
      Alcotest.(check bool) "stream consumed" res2 true
      ; Alcotest.(check int) "(producer & consumer)" !last 1
      ; Lwt.return_unit

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
      Bytes.blit_string x 0 bytes off len
      ; if len = String.length x then inputs := r
        else inputs := String.sub x len (String.length x - len) :: r
      ; inj (Lwt.return (`Len len)) in
  let rec wr () bytes off len =
    match !outputs with
    | [] -> Fmt.failwith "Unexpected output: %S" (String.sub bytes off len)
    | x :: r ->
      let max = len in
      let len = min (String.length x) len in
      if String.sub x 0 len <> String.sub bytes off len then
        Fmt.failwith "Expected %S, have %S" (String.sub x 0 len)
          (String.sub bytes off len)
      ; if String.length x = len then outputs := r
        else outputs := String.sub x len (String.length x - len) :: r
      ; if len < max then wr () bytes (off + len) (max - len)
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

let load_file filename =
  let open Rresult in
  Bos.OS.File.read filename >>= fun contents ->
  R.ok (Cstruct.of_string contents)

let cert =
  let open Rresult in
  load_file (Fpath.v "server.pem") >>= fun raw ->
  X509.Certificate.decode_pem raw

let cert = Rresult.R.get_ok cert

let private_key =
  let open Rresult in
  load_file (Fpath.v "server.key") >>= fun raw ->
  X509.Private_key.decode_pem raw

let private_key = Rresult.R.get_ok private_key

let fake_tls_config =
  Tls.Config.server
    ~certificates:(`Single ([cert], private_key))
    ~authenticator:(fun ~host:_ _ -> Ok None)
    ()

let smtp_test_0 =
  Alcotest_lwt.test_case "SMTP (relay) 0" `Quick @@ fun _sw () ->
  let rdwr, check = rdwr_from_flows [] ["220 x25519.net"] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain= x25519
    ; ipv4= Ipaddr.V4.localhost
    ; tls= fake_tls_config
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 0L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_relay_init ctx info) rdwr >>= function
  | Ok _ -> Alcotest.fail "Unexpected good result"
  | Error (`Error (`Protocol `End_of_input)) ->
    Alcotest.(check unit) "empty stream" (check ()) ()
    ; Alcotest.(check pass) "connection close" () ()
    ; Lwt.return_unit
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
    ; ipv4= Ipaddr.V4.localhost
    ; tls= fake_tls_config
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 16777216L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_relay_init ctx info) rdwr >>= function
  | Ok `Quit ->
    Alcotest.(check unit) "empty stream" (check ()) ()
    ; Alcotest.(check pass) "quit" () ()
    ; Lwt.return_unit
  | Ok (`Submission _) -> Alcotest.fail "Unexpected submission"
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
    ; ipv4= Ipaddr.V4.localhost
    ; tls= fake_tls_config
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 16777216L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_relay_init ctx info) rdwr >>= function
  | Ok `Quit ->
    Alcotest.(check unit) "empty stream" (check ()) ()
    ; Alcotest.(check pass) "quit" () ()
    ; Lwt.return_unit
  | Ok (`Submission _) -> Alcotest.fail "Unexpected submission"
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
    ; ipv4= Ipaddr.V4.localhost
    ; tls= fake_tls_config
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 16777216L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_relay_init ctx info) rdwr >>= function
  | Ok (`Quit | `Submission _) -> Alcotest.fail "Unexpected quit or submission"
  | Error (`Error `Too_many_bad_commands) ->
    Alcotest.(check unit) "empty stream" (check ()) ()
    ; Alcotest.(check pass) "too many bad commands" () ()
    ; Lwt.return_unit
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
    ; ipv4= Ipaddr.V4.localhost
    ; tls= fake_tls_config
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 16777216L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_relay_init ctx info) rdwr >>= function
  | Ok _ -> Alcotest.fail "Unexpected quit or submission"
  | Error (`Error `No_recipients) ->
    Alcotest.(check unit) "empty stream" (check ()) ()
    ; Alcotest.(check pass) "no recipients" () ()
    ; Lwt.return_unit
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
    ; ipv4= Ipaddr.V4.localhost
    ; tls= fake_tls_config
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 16777216L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_relay_init ctx info) rdwr >>= function
  | Ok
      (`Submission
        {Ptt.SSMTP.from; Ptt.SSMTP.recipients; Ptt.SSMTP.domain_from}) ->
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
      ((Rresult.R.get_ok <.> Colombe_emile.to_reverse_path) romain_calascibetta)
    ; Alcotest.(check (list forward_path))
        "recipients" (List.map fst recipients)
        [(Rresult.R.get_ok <.> Colombe_emile.to_forward_path) anil]
    ; Alcotest.(check domain)
        "domain" domain_from
        ((Rresult.R.get_ok <.> Colombe_emile.to_domain) gmail)
    ; Alcotest.(check unit) "empty stream" (check ()) ()
    ; Alcotest.(check pass) "submission" () ()
    ; Lwt.return_unit
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
    ; ipv4= Ipaddr.V4.localhost
    ; tls= fake_tls_config
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 16777216L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_submission_init ctx info [Ptt.Mechanism.PLAIN]) rdwr
  >>= function
  | Ok `Quit ->
    Alcotest.(check unit) "empty stream" (check ()) ()
    ; Alcotest.(check pass) "quit" () ()
    ; Lwt.return_unit
  | Ok (`Authentication _) -> Alcotest.failf "Unexpected authentication"
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
    ; ipv4= Ipaddr.V4.localhost
    ; tls= fake_tls_config
    ; zone= Mrmime.Date.Zone.gmt
    ; size= 16777216L
    } in
  let open Lwt.Infix in
  run_state (Ptt.SSMTP.m_submission_init ctx info [Ptt.Mechanism.PLAIN]) rdwr
  >>= function
  | Ok (`Authentication (v, m)) ->
    let gmail =
      let open Mrmime.Mailbox in
      Domain.(v domain [a "gmail"; a "com"]) in
    Alcotest.(check unit) "empty stream" (check ()) ()
    ; Alcotest.(check mechanism) "mechanism" m Ptt.Mechanism.PLAIN
    ; Alcotest.(check domain)
        "domain" v
        ((Rresult.R.get_ok <.> Colombe_emile.to_domain) gmail)
    ; Alcotest.(check pass) "authentication" () ()
    ; Lwt.return_unit
  | Ok `Quit | Ok (`Submission _) ->
    Alcotest.failf "Unexpected quit or submission"
  | Error (`Error err) ->
    Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.failf "Unexpected connection close"

module Random = struct
  type g = unit
  type +'a io = 'a Lwt.t

  let generate ?g:_ _ = Lwt.return ()
end

let fake_dns_resolvers = Hashtbl.create 16

let () =
  Hashtbl.add fake_dns_resolvers
    (Domain_name.(host_exn <.> of_string_exn) "gmail.com")
    (Ipaddr.V4.of_string_exn "10.0.0.8")

let fake_smtp_servers = Hashtbl.create 16
let () = Hashtbl.add fake_smtp_servers (Ipaddr.of_string_exn "10.0.0.8") 8888

module Resolver = struct
  type t = unit
  type +'a io = 'a Lwt.t

  let gethostbyname () domain_name =
    match Hashtbl.find fake_dns_resolvers domain_name with
    | v -> Lwt.return (Ok v)
    | exception Not_found ->
      let err = Rresult.R.error_msgf "%a not found" Domain_name.pp domain_name in
      Lwt.return err

  let getmxbyname () domain_name =
    let mxs =
      Dns.Rr_map.Mx_set.add
        {Dns.Mx.preference= 0; mail_exchange= domain_name}
        Dns.Rr_map.Mx_set.empty in
    Lwt.return (Ok mxs)

  let extension () _ _ =
    let err = Rresult.R.error_msgf "Extension are not available" in
    Lwt.return err
end

module Flow = struct
  type t = Lwt_unix.file_descr
  type +'a io = 'a Lwt.t

  let flow endpoint =
    let sockaddr =
      match endpoint with
      | Unix.ADDR_UNIX _ -> endpoint
      | Unix.ADDR_INET (fake_inet_addr, _) ->
        let fake_inet_addr = Ipaddr_unix.of_inet_addr fake_inet_addr in
        Unix.ADDR_INET
          ( Unix.inet_addr_loopback
          , Hashtbl.find fake_smtp_servers fake_inet_addr ) in
    let open Lwt.Infix in
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.connect socket sockaddr >>= fun () -> Lwt.return socket

  let recv socket buf off len =
    Lwt.catch
      (fun () -> Lwt_unix.read socket buf off len)
      (fun exn ->
        Logs.err (fun m ->
            m "[recv] Got an exception: %S." (Printexc.to_string exn))
        ; Lwt.fail exn)

  let send socket buf off len =
    let open Lwt.Infix in
    let rec go socket buf off len =
      if len > 0 then
        Lwt.catch
          (fun () ->
            Lwt_unix.write socket buf off len >>= fun res ->
            go socket buf (off + res) (len - res))
          (fun exn ->
            Logs.err (fun m ->
                m "[send] Got an exception: %S." (Printexc.to_string exn))
            ; Lwt.fail exn)
      else Lwt.return_unit in
    go socket (Bytes.unsafe_of_string buf) off len
end

let serve_when_ready ?stop ~handler socket =
  let open Lwt.Infix in
  `Initialized
    (let switched_off =
       let t, u = Lwt.wait () in
       Lwt_switch.add_hook stop (fun () ->
           Lwt.wakeup_later u `Stopped
           ; Lwt.return_unit)
       ; t in
     let rec loop () =
       Lwt_unix.accept socket >>= fun (flow, _) ->
       Lwt.async (fun () -> handler flow)
       ; Lwt.pause () >>= loop in
     let stop =
       Lwt.pick [switched_off; loop ()] >>= fun `Stopped ->
       Lwt_unix.close socket in
     stop)

let make_submission_smtp_server ?stop ~port info =
  let module SMTP =
    Ptt.Relay.Make (Scheduler) (Lwt_io) (Flow) (Resolver) (Random)
  in
  let conf_server = SMTP.create ~info in
  let messaged = SMTP.messaged conf_server in
  let smtp_submission_server conf_server =
    let open Lwt.Infix in
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let sockaddr =
      Unix.ADDR_INET (Ipaddr_unix.to_inet_addr (Ipaddr.V4 info.SMTP.ipv4), port)
    in
    Lwt_unix.bind socket sockaddr >|= fun () ->
    Lwt_unix.listen socket 40

    ; let handler flow =
        let open Lwt.Infix in
        SMTP.accept flow () conf_server >>= fun res ->
        Lwt_unix.close flow >>= fun () ->
        match res with Ok _ -> Lwt.return () | Error _err -> Lwt.return () in
      serve_when_ready ?stop ~handler socket in
  let smtp_logic messaged ms =
    let open Lwt.Infix in
    Lwt.return
      (`Queue
        (let th, u = Lwt.wait () in
         Lwt_switch.add_hook stop (fun () ->
             Lwt.wakeup_later u `Stopped
             ; Lwt.return_unit)
         ; let rec loop () =
             SMTP.Md.await messaged >>= fun () ->
             SMTP.Md.pop messaged >>= function
             | Some (key, queue, _) ->
               SMTP.Md.close queue >>= fun () -> Queue.push key ms ; loop ()
             | None -> loop () in
           Lwt.pick [th; loop ()] >|= fun `Stopped ->
           Queue.fold (rev List.cons) [] ms)) in
  Lwt.both
    (smtp_submission_server conf_server)
    (smtp_logic messaged (Queue.create ()))

let sendmail ipv4 port ~domain sender recipients contents =
  let stream = stream_of_string_list contents in
  let stream () =
    let open Lwt.Infix in
    stream () >>= function
    | Some str -> Lwt.return (Some (str ^ "\r\n", 0, String.length str + 2))
    | None -> Lwt.return None in
  let stream = Scheduler.inj <.> stream in
  let tls_config =
    Tls.Config.client ~authenticator:(fun ~host:_ _ -> Ok None) () in
  let ctx = Sendmail_with_starttls.Context_with_tls.make () in
  let rdwr =
    {
      Colombe.Sigs.rd=
        (fun fd buf off len ->
          let fiber =
            Lwt.Infix.(
              Lwt_unix.read fd buf off len >>= function
              | 0 -> Lwt.return `End
              | len -> Lwt.return (`Len len)) in
          Scheduler.inj fiber)
    ; Colombe.Sigs.wr=
        (fun fd buf off len ->
          let fiber =
            let open Lwt.Infix in
            Lwt_unix.write fd (Bytes.unsafe_of_string buf) off len >>= fun _ ->
            Lwt.return_unit in
          Scheduler.inj fiber)
    } in
  let open Lwt.Infix in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket
    (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr (Ipaddr.V4 ipv4), port))
  >>= fun () ->
  let res =
    Sendmail_with_starttls.sendmail lwt rdwr socket ctx tls_config ~domain
      sender recipients stream in
  let open Lwt.Infix in
  Scheduler.prj res >>= function
  | Ok () -> Lwt_unix.close socket
  | Error err -> Fmt.failwith "%a" Sendmail_with_starttls.pp_error err

let key = Alcotest.testable Ptt.Messaged.pp Ptt.Messaged.equal

let full_test_0 =
  Alcotest_lwt.test_case "Receive one email from Anil" `Quick @@ fun _sw () ->
  let romain_calascibetta =
    (Rresult.R.get_ok <.> Colombe_emile.to_forward_path)
      (let open Mrmime.Mailbox in
      Local.[w "romain"; w "calascibetta"]
      @ Domain.(domain, [a "gmail"; a "com"])) in
  let anil =
    (Rresult.R.get_ok <.> Colombe_emile.to_reverse_path)
      (let open Mrmime.Mailbox in
      Local.[w "anil"] @ Domain.(domain, [a "recoil"; a "org"])) in
  let recoil = (Colombe.Domain.of_string_exn <.> Domain_name.to_string) recoil in
  let sendmail contents =
    sendmail Ipaddr.V4.localhost 8888 ~domain:recoil anil [romain_calascibetta]
      contents in
  let stop = Lwt_switch.create () in
  let open Lwt.Infix in
  make_submission_smtp_server ~stop ~port:8888
    {
      Ptt.SMTP.domain= gmail
    ; ipv4= Ipaddr.V4.localhost
    ; tls= fake_tls_config
    ; zone= Mrmime.Date.Zone.GMT
    ; size= 0x1000000L
    }
  >>= fun (`Initialized th0, `Queue th1) ->
  Lwt.join
    [
      ( sendmail
          [
            "From: anil@recoil.org"; "Subject: SMTP server, PLZ!"; ""
          ; "Hello World!"
          ]
      >>= fun () -> Lwt_switch.turn_off stop ); th0
    ]
  >>= fun () ->
  th1 >>= fun contents ->
  Alcotest.(check (list key))
    "inbox" contents
    [
      Ptt.Messaged.v ~domain_from:recoil ~from:(anil, [])
        ~recipients:[romain_calascibetta, []] 0L
    ]
  ; Lwt.return_unit

let full_test_1 =
  Alcotest_lwt.test_case "Receive emails from Anil and Thomas" `Quick
  @@ fun _sw () ->
  let romain_calascibetta =
    (Rresult.R.get_ok <.> Colombe_emile.to_forward_path)
      (let open Mrmime.Mailbox in
      Local.[w "romain"; w "calascibetta"]
      @ Domain.(domain, [a "gmail"; a "com"])) in
  let anil =
    (Rresult.R.get_ok <.> Colombe_emile.to_reverse_path)
      (let open Mrmime.Mailbox in
      Local.[w "anil"] @ Domain.(domain, [a "recoil"; a "org"])) in
  let thomas =
    (Rresult.R.get_ok <.> Colombe_emile.to_reverse_path)
      (let open Mrmime.Mailbox in
      Local.[w "thomas"] @ Domain.(domain, [a "gazagnaire"; a "org"])) in
  let recoil = (Colombe.Domain.of_string_exn <.> Domain_name.to_string) recoil in
  let gazagnaire =
    (Colombe.Domain.of_string_exn <.> Domain_name.to_string) gazagnaire in
  let stop = Lwt_switch.create () in
  let open Lwt.Infix in
  make_submission_smtp_server ~stop ~port:8888
    {
      Ptt.SMTP.domain= gmail
    ; ipv4= Ipaddr.V4.localhost
    ; tls= fake_tls_config
    ; zone= Mrmime.Date.Zone.GMT
    ; size= 0x1000000L
    }
  >>= fun (`Initialized th0, `Queue th1) ->
  let sendmail ~domain sender contents =
    sendmail Ipaddr.V4.localhost 8888 ~domain sender [romain_calascibetta]
      contents in
  Lwt.join
    [
      ( sendmail ~domain:recoil anil
          [
            "From: anil@recoil.org"; "Subject: SMTP server, PLZ!"; ""
          ; "Hello World!"
          ]
      >>= fun () ->
        sendmail ~domain:gazagnaire thomas
          [
            "From: anil@recoil.org"; "Subject: SMTP server, PLZ!"; ""
          ; "Hello World!"
          ]
        >>= fun () -> Lwt_switch.turn_off stop ); th0
    ]
  >>= fun () ->
  th1 >>= fun contents ->
  Alcotest.(check (list key))
    "inbox" contents
    [
      Ptt.Messaged.v ~domain_from:gazagnaire ~from:(thomas, [])
        ~recipients:[romain_calascibetta, []] 1L
    ; Ptt.Messaged.v ~domain_from:recoil ~from:(anil, [])
        ~recipients:[romain_calascibetta, []] 0L
    ]
  ; Lwt.return_unit

let fiber =
  Alcotest_lwt.run "ptt"
    [
      "mechanism", [mechanism_test_0]; "authentication", [authentication_test_0]
    ; "aggregate", [aggregate_test_0]
    ; "messaged", [messaged_test_0; messaged_test_1]
    ; ( "SMTP"
      , [
          smtp_test_0; smtp_test_1; smtp_test_2; smtp_test_3; smtp_test_4
        ; smtp_test_5; smtp_test_6; smtp_test_7
        ] ); "Server", [full_test_0; full_test_1]
    ]

let () = Lwt_main.run fiber
