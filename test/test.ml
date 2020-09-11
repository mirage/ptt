let () = Printexc.record_backtrace true

let reporter = Logs_fmt.reporter ()

let () = Fmt.set_utf_8 Fmt.stdout true

let () = Fmt.set_utf_8 Fmt.stderr true

let () = Fmt.set_style_renderer Fmt.stdout `Ansi_tty

let () = Fmt.set_style_renderer Fmt.stderr `Ansi_tty

let () = Logs.set_level ~all:true (Some Logs.Debug)

let () = Logs.set_reporter reporter

let () = Mirage_crypto_rng_unix.initialize ()

open Tcp

module Unix_scheduler = Colombe.Sigs.Make (struct
  type +'a t = 'a
end)

let unix =
  let open Unix_scheduler in
  {
    Colombe.Sigs.bind = (fun x f -> f (prj x));
    Colombe.Sigs.return = (fun x -> inj x);
  }

module IO = struct
  type +'a t = 'a

  module Mutex = struct
    type +'a fiber = 'a

    type t = Mutex.t

    let create () = Mutex.create ()

    let lock t = Mutex.lock t

    let unlock t = Mutex.unlock t
  end

  module Condition = struct
    type +'a fiber = 'a

    type mutex = Mutex.t

    type t = Condition.t

    let create () = Condition.create ()

    let wait mutex t = Condition.wait mutex t

    let signal t = Condition.signal t

    let broadcast t = Condition.broadcast t
  end

  let bind x f = f x

  let return x = x
end

let ( <.> ) f g x = f (g x)

let rev f a b = f b a

let mechanism = Alcotest.testable Ptt.Mechanism.pp Ptt.Mechanism.equal

let msg =
  Alcotest.testable Rresult.R.pp_msg (fun (`Msg a) (`Msg b) -> String.equal a b)

let mechanism_test_0 =
  Alcotest.test_case "mechanism 0" `Quick @@ fun () ->
  Alcotest.(check mechanism)
    "plain"
    (Ptt.Mechanism.of_string_exn "plain")
    Ptt.Mechanism.PLAIN ;
  Alcotest.(check mechanism)
    "PLAIN"
    (Ptt.Mechanism.of_string_exn "PLAIN")
    Ptt.Mechanism.PLAIN ;
  Alcotest.(check mechanism)
    "PlAiN"
    (Ptt.Mechanism.of_string_exn "PlAiN")
    Ptt.Mechanism.PLAIN ;
  Alcotest.check_raises "PLAIZ" (Invalid_argument "Invalid mechanism: PLAIZ")
    (fun () -> ignore @@ Ptt.Mechanism.of_string_exn "PLAIZ")

let auth0 =
  let module Map = Map.Make (struct
    type t = Emile.local

    let compare = Emile.compare_local ~case_sensitive:false
  end) in
  let open Mrmime.Mailbox in
  let m =
    Map.empty
    |> Map.add
         Local.(v [ w "romain"; w "calascibetta" ])
         Digestif.(digest_string SHA1 "toto")
    |> Map.add Local.(v [ w "thomas" ]) Digestif.(digest_string SHA1 "tata")
    |> Map.add Local.(v [ w "anil" ]) Digestif.(digest_string SHA1 "tutu")
    |> Map.add Local.(v [ w "hannes" ]) Digestif.(digest_string SHA1 "titi")
    |> Map.add Local.(v [ w "gemma" ]) Digestif.(digest_string SHA1 "") in
  let f username password =
    match Map.find username m with
    | v -> Unix_scheduler.inj Digestif.(equal SHA1 password v)
    | exception Not_found -> Unix_scheduler.inj false in
  Ptt.Authentication.v f

let authentication_test_0 =
  Alcotest.test_case "authentication 0" `Quick @@ fun () ->
  let auth hash mechanism authenticator fmt =
    Fmt.kstrf
      (fun payload ->
        Ptt.Authentication.decode_authentication unix hash mechanism
          authenticator
          (Base64.encode_exn payload)
        |> Unix_scheduler.prj)
      fmt in
  let plain_none = Ptt.Authentication.PLAIN None in
  Alcotest.(check (result bool msg))
    "romain" (Ok true)
    (auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "romain.calascibetta"
       "toto") ;
  Alcotest.(check (result bool msg))
    "thomas" (Ok true)
    (auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "thomas" "tata") ;
  Alcotest.(check (result bool msg))
    "anil" (Ok true)
    (auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "anil" "tutu") ;
  Alcotest.(check (result bool msg))
    "hannes" (Ok true)
    (auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "hannes" "titi") ;
  Alcotest.(check (result bool msg))
    "gemma" (Ok true)
    (auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "gemma" "") ;
  Alcotest.(check (result bool msg))
    "romain (wrong)" (Ok false)
    (auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "romain.calascibetta"
       "titi") ;
  Alcotest.(check (result bool msg))
    "pierre" (Ok false)
    (auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "pierre.caillou" "toto") ;
  Alcotest.(check (result bool msg))
    "bad stamp"
    (Error (`Msg "Unexpected stamp"))
    (auth Digestif.SHA1 plain_none auth0 "stamp\000%s\000%s"
       "romain.calascibetta" "toto") ;
  Alcotest.(check (result bool msg))
    "malformed"
    (Error (`Msg "Invalid input"))
    (auth Digestif.SHA1 plain_none auth0 "salut les copains") ;
  Alcotest.(check (result bool msg))
    "no stamp"
    (Error (`Msg "Invalid stamp"))
    (auth Digestif.SHA1 (Ptt.Authentication.PLAIN (Some "stamp")) auth0
       "\000%s\000%s" "anil" "tutu") ;
  Alcotest.(check (result bool msg))
    "invalid username"
    (Error (`Msg "Invalid username: \"\""))
    (auth Digestif.SHA1 plain_none auth0 "\000\000%s" "tutu")

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
  match (a, b) with
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
  Alcotest.test_case "aggregate 0" `Quick @@ fun () ->
  let open Mrmime.Mailbox in
  let m0 =
    Local.[ w "romain"; w "calascibetta" ]
    @ Domain.(domain, [ a "gmail"; a "com" ]) in
  let m1 = Local.[ w "thomas" ] @ Domain.(domain, [ a "gazagnaire"; a "org" ]) in
  let m2 = Local.[ w "anil" ] @ Domain.(domain, [ a "recoil"; a "org" ]) in
  let m3 =
    Local.[ w "gemma"; w "d"; w "gordon" ]
    @ Domain.(domain, [ a "gmail"; a "com" ]) in
  let ms =
    List.map
      (Rresult.R.get_ok <.> Colombe_emile.to_forward_path)
      [ m0; m1; m2; m3 ] in
  let ms =
    Colombe.Forward_path.Domain
      ((Rresult.R.get_ok <.> Colombe_emile.to_domain)
         Domain.(v domain [ a "nqsb"; a "io" ]))
    :: ms in
  let u, r = Ptt.Aggregate.aggregate_by_domains ~domain:x25519 ms in
  Alcotest.(check bool)
    "resolved is empty" true
    (Ptt.Aggregate.By_ipaddr.is_empty r) ;
  Alcotest.(check unresolved)
    "unresolved gmail.com"
    (`Local
      [
        Local.(v [ w "romain"; w "calascibetta" ]);
        Local.(v [ w "gemma"; w "d"; w "gordon" ]);
      ])
    (Ptt.Aggregate.By_domain.find gmail u) ;
  Alcotest.(check unresolved)
    "unresolved recoil.org"
    (`Local [ Local.(v [ w "anil" ]) ])
    (Ptt.Aggregate.By_domain.find recoil u) ;
  Alcotest.(check unresolved)
    "unresolved gazagnaire.org"
    (`Local [ Local.(v [ w "thomas" ]) ])
    (Ptt.Aggregate.By_domain.find gazagnaire u) ;
  Alcotest.(check unresolved)
    "unresolved nqsb.io" `All
    (Ptt.Aggregate.By_domain.find nqsb u)

module Md = Ptt.Messaged.Make (Unix_scheduler) (IO)

let stream_of_string_list l =
  let l = ref l in
  let stream () =
    match !l with
    | [] -> None
    | x :: r ->
        l := r ;
        Some x in
  stream

let stream_is_empty s = match s () with Some _ -> false | None -> true

let hello_world () =
  stream_of_string_list [ ("Hello", 0, 5); (" ", 0, 1); ("World!", 0, 6) ]

let hello_buddy () =
  stream_of_string_list [ ("Hello", 0, 5); (" ", 0, 1); ("buddy!", 0, 6) ]

let hello_guy () =
  stream_of_string_list [ ("Hello", 0, 5); (" ", 0, 1); ("guy!", 0, 4) ]

let messaged_test_0 =
  Alcotest.test_case "messaged 0" `Quick @@ fun () ->
  let md = Md.create () in
  let do0 ~domain_from ~from v () =
    let key =
      Ptt.Messaged.v
        ~domain_from:
          ((Rresult.R.get_ok <.> Colombe_emile.to_domain) domain_from)
        ~from:((Rresult.R.get_ok <.> Colombe_emile.to_reverse_path) from, [])
        ~recipients:[] 0L in
    let producer = Md.push md key in
    let v = v () (* XXX(dinosaure): really create the stream. *) in
    let rec consume () =
      match v () with
      | Some chunk ->
          producer (Some chunk) ;
          consume ()
      | None -> producer None in
    consume () in
  let contents = ref "" in
  let do1 () =
    Md.await md ;
    (* XXX(dinosaure): schedule [do1] __after__ [do0]. *)
    match Md.pop md with
    | Some (_, _, v) ->
        let buf = Buffer.create 0x100 in
        let rec consume () =
          match v () with
          | Some (str, off, len) ->
              Buffer.add_substring buf str off len ;
              consume ()
          | None -> contents := Buffer.contents buf in
        consume ()
    | None -> assert false in
  let domain_from = Mrmime.Mailbox.Domain.(v domain [ a "x25519"; a "net" ]) in
  let from =
    let open Mrmime.Mailbox in
    Local.[ w "romain"; w "calascibetta" ]
    @ Domain.(domain, [ a "gmail"; a "com" ]) in

  let th0 = Thread.create (do0 ~domain_from ~from hello_world) () in
  let th1 = Thread.create do1 () in
  Thread.join th0 ;
  Thread.join th1 ;
  Alcotest.(check string) "(random schedule) payload" !contents "Hello World!" ;
  let th1 = Thread.create do1 () in
  Unix.sleep 1 ;
  let th0 = Thread.create (do0 ~domain_from ~from hello_buddy) () in
  Thread.join th0 ;
  Thread.join th1 ;
  Alcotest.(check string)
    "(consumer & producer) payload" !contents "Hello buddy!" ;
  let th0 = Thread.create (do0 ~domain_from ~from hello_guy) () in
  Unix.sleep 1 ;
  let th1 = Thread.create do1 () in
  Thread.join th0 ;
  Thread.join th1 ;
  Alcotest.(check string) "(producer & consumer) payload" !contents "Hello guy!"

let messaged_test_1 =
  Alcotest.test_case "messaged 1" `Quick @@ fun () ->
  let md = Md.create () in
  let last = ref 0 in
  let do0 ~domain_from ~from v () =
    last := 0 ;
    let key =
      Ptt.Messaged.v
        ~domain_from:
          ((Rresult.R.get_ok <.> Colombe_emile.to_domain) domain_from)
        ~from:((Rresult.R.get_ok <.> Colombe_emile.to_reverse_path) from, [])
        ~recipients:[] 0L in
    let producer = Md.push md key in
    let rec consume () =
      match v () with
      | Some chunk ->
          producer (Some chunk) ;
          consume ()
      | None -> producer None in
    consume () in
  let do1 () =
    last := 1 ;
    Md.await md ;
    (* XXX(dinosaure): schedule [do1] __after__ [do0]. *)
    match Md.pop md with
    | Some (_, q, _) -> Md.close q (* XXX(dinosaure): unlock [do0]. *)
    | None -> assert false in
  let domain_from = Mrmime.Mailbox.Domain.(v domain [ a "x25519"; a "net" ]) in
  let from =
    let open Mrmime.Mailbox in
    Local.[ w "romain"; w "calascibetta" ]
    @ Domain.(domain, [ a "gmail"; a "com" ]) in

  let stream = hello_world () in
  let th0 = Thread.create (do0 ~domain_from ~from stream) () in
  let th1 = Thread.create do1 () in
  Thread.join th0 ;
  Thread.join th1 ;
  Alcotest.(check bool) "stream consumed" (stream_is_empty stream) true ;
  Alcotest.(check pass) "random schedule" () () ;
  let th1 = Thread.create do1 () in
  Unix.sleep 1 ;
  let stream = hello_buddy () in
  let th0 = Thread.create (do0 ~domain_from ~from stream) () in
  Thread.join th0 ;
  Thread.join th1 ;
  Alcotest.(check bool) "stream consumed" (stream_is_empty stream) true ;
  Alcotest.(check int) "(consumer & producer)" !last 0 ;
  let stream = hello_guy () in
  let th0 = Thread.create (do0 ~domain_from ~from stream) () in
  Unix.sleep 1 ;
  let th1 = Thread.create do1 () in
  Thread.join th0 ;
  Thread.join th1 ;
  Alcotest.(check bool) "stream consumed" (stream_is_empty stream) true ;
  Alcotest.(check int) "(producer & consumer)" !last 1

let put_crlf x = x ^ "\r\n"

let rdwr_from_flows inputs outputs =
  let inputs = ref (List.map put_crlf inputs) in
  let outputs = ref (List.map put_crlf outputs) in
  let open Unix_scheduler in
  let rd () bytes off len =
    match !inputs with
    | [] -> inj 0
    | x :: r ->
        let len = min (String.length x) len in
        Bytes.blit_string x 0 bytes off len ;
        if len = String.length x
        then inputs := r
        else inputs := String.sub x len (String.length x - len) :: r ;
        inj len in
  let rec wr () bytes off len =
    match !outputs with
    | [] -> Fmt.failwith "Unexpected output: %S" (String.sub bytes off len)
    | x :: r ->
        let max = len in
        let len = min (String.length x) len in
        if String.sub x 0 len <> String.sub bytes off len
        then
          Fmt.failwith "Expected %S, have %S" (String.sub x 0 len)
            (String.sub bytes off len) ;
        if String.length x = len
        then outputs := r
        else outputs := String.sub x len (String.length x - len) :: r ;
        if len < max then wr () bytes (off + len) (max - len) else inj () in
  ( { Colombe.Sigs.rd; Colombe.Sigs.wr },
    fun () ->
      match (!inputs, !outputs) with
      | [], [] -> ()
      | r, w ->
          Fmt.failwith
            "inputs or outputs are not empty: @[<hov>%a@] and @[<hov>%a@]"
            Fmt.(Dump.list string)
            r
            Fmt.(Dump.list string)
            w )

let run m rdwr () =
  let rec go = function
    | Colombe.State.Write { buffer; off; len; k } ->
        let () = rdwr.Colombe.Sigs.wr () buffer off len |> Unix_scheduler.prj in
        go (k len)
    | Colombe.State.Return v -> Ok v
    | Colombe.State.Error err -> Error (`Error err)
    | Colombe.State.Read { buffer; off; len; k } ->
    match rdwr.Colombe.Sigs.rd () buffer off len |> Unix_scheduler.prj with
    | 0 -> Rresult.R.error `Connection_close
    | n -> (go <.> k) n in
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
  X509.Private_key.decode_pem raw >>= fun (`RSA v) -> R.ok v

let private_key = Rresult.R.get_ok private_key

let fake_tls_config =
  Tls.Config.server
    ~certificates:(`Single ([ cert ], private_key))
    ~authenticator:(fun ~host:_ _ -> Ok None)
    ()

let smtp_test_0 =
  Alcotest.test_case "SMTP (relay) 0" `Quick @@ fun () ->
  let rdwr, check = rdwr_from_flows [] [ "220 x25519.net" ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain = x25519;
      ipv4 = Ipaddr.V4.localhost;
      tls = fake_tls_config;
      zone = Mrmime.Date.Zone.gmt;
      size = 0L;
    } in
  match run (Ptt.SSMTP.m_relay_init ctx info) rdwr () with
  | Ok _ -> Alcotest.fail "Unexpected good result"
  | Error (`Error _) -> Alcotest.fail "Unexpected protocol error"
  | Error `Connection_close ->
      Alcotest.(check unit) "empty stream" (check ()) () ;
      Alcotest.(check pass) "connection close" () ()

let smtp_test_1 =
  Alcotest.test_case "SMTP (relay) 1" `Quick @@ fun () ->
  let rdwr, check =
    rdwr_from_flows
      [ "EHLO gmail.com"; "QUIT" ]
      [
        "220 x25519.net";
        "250-x25519.net at your service, [127.0.0.1]";
        "250-8BITMIME";
        "250-SMTPUTF8";
        "250 SIZE 16777216";
        "221 Bye, buddy!";
      ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain = x25519;
      ipv4 = Ipaddr.V4.localhost;
      tls = fake_tls_config;
      zone = Mrmime.Date.Zone.gmt;
      size = 16777216L;
    } in
  match run (Ptt.SSMTP.m_relay_init ctx info) rdwr () with
  | Ok `Quit ->
      Alcotest.(check unit) "empty stream" (check ()) () ;
      Alcotest.(check pass) "quit" () ()
  | Ok (`Submission _) -> Alcotest.fail "Unexpected submission"
  | Error (`Error err) ->
      Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.fail "Unexpected connection close"

let smtp_test_2 =
  Alcotest.test_case "SMTP (relay) 2" `Quick @@ fun () ->
  let rdwr, check =
    rdwr_from_flows
      [ "EHLO gmail.com"; "RSET"; "QUIT" ]
      [
        "220 x25519.net";
        "250-x25519.net at your service, [127.0.0.1]";
        "250-8BITMIME";
        "250-SMTPUTF8";
        "250 SIZE 16777216";
        "250 Yes buddy!";
        "221 Bye, buddy!";
      ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain = x25519;
      ipv4 = Ipaddr.V4.localhost;
      tls = fake_tls_config;
      zone = Mrmime.Date.Zone.gmt;
      size = 16777216L;
    } in
  match run (Ptt.SSMTP.m_relay_init ctx info) rdwr () with
  | Ok `Quit ->
      Alcotest.(check unit) "empty stream" (check ()) () ;
      Alcotest.(check pass) "quit" () ()
  | Ok (`Submission _) -> Alcotest.fail "Unexpected submission"
  | Error (`Error err) ->
      Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.fail "Unexpected connection close"

let smtp_test_3 =
  Alcotest.test_case "SMTP (relay) 3" `Quick @@ fun () ->
  let rdwr, check =
    rdwr_from_flows
      [
        "EHLO gmail.com";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
        "RSET";
      ]
      [
        "220 x25519.net";
        "250-x25519.net at your service, [127.0.0.1]";
        "250-8BITMIME";
        "250-SMTPUTF8";
        "250 SIZE 16777216";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "250 Yes buddy!";
        "554 You reached the limit buddy!";
      ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain = x25519;
      ipv4 = Ipaddr.V4.localhost;
      tls = fake_tls_config;
      zone = Mrmime.Date.Zone.gmt;
      size = 16777216L;
    } in
  match run (Ptt.SSMTP.m_relay_init ctx info) rdwr () with
  | Ok (`Quit | `Submission _) -> Alcotest.fail "Unexpected quit or submission"
  | Error (`Error `Too_many_bad_commands) ->
      Alcotest.(check unit) "empty stream" (check ()) () ;
      Alcotest.(check pass) "too many bad commands" () ()
  | Error (`Error err) ->
      Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.fail "Unexpected connection close"

let smtp_test_4 =
  Alcotest.test_case "SMTP (relay) 4" `Quick @@ fun () ->
  let rdwr, check =
    rdwr_from_flows
      [ "EHLO gmail.com"; "MAIL FROM:<romain.calascibetta@gmail.com>"; "DATA" ]
      [
        "220 x25519.net";
        "250-x25519.net at your service, [127.0.0.1]";
        "250-8BITMIME";
        "250-SMTPUTF8";
        "250 SIZE 16777216";
        "250 Ok, buddy!";
        "554 No recipients";
      ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain = x25519;
      ipv4 = Ipaddr.V4.localhost;
      tls = fake_tls_config;
      zone = Mrmime.Date.Zone.gmt;
      size = 16777216L;
    } in
  match run (Ptt.SSMTP.m_relay_init ctx info) rdwr () with
  | Ok _ -> Alcotest.fail "Unexpected quit or submission"
  | Error (`Error `No_recipients) ->
      Alcotest.(check unit) "empty stream" (check ()) () ;
      Alcotest.(check pass) "no recipients" () ()
  | Error (`Error err) ->
      Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.fail "Unexpected connection close"

let reverse_path =
  Alcotest.testable Colombe.Reverse_path.pp Colombe.Reverse_path.equal

let forward_path =
  Alcotest.testable Colombe.Forward_path.pp Colombe.Forward_path.equal

let domain = Alcotest.testable Colombe.Domain.pp Colombe.Domain.equal

let smtp_test_5 =
  Alcotest.test_case "SMTP (relay) 5" `Quick @@ fun () ->
  let rdwr, check =
    rdwr_from_flows
      [
        "EHLO gmail.com";
        "MAIL FROM:<romain.calascibetta@gmail.com>";
        "RCPT TO:<anil@recoil.org>";
        "DATA";
      ]
      [
        "220 x25519.net";
        "250-x25519.net at your service, [127.0.0.1]";
        "250-8BITMIME";
        "250-SMTPUTF8";
        "250 SIZE 16777216";
        "250 Ok, buddy!";
        "250 Ok, buddy!";
      ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain = x25519;
      ipv4 = Ipaddr.V4.localhost;
      tls = fake_tls_config;
      zone = Mrmime.Date.Zone.gmt;
      size = 16777216L;
    } in
  match run (Ptt.SSMTP.m_relay_init ctx info) rdwr () with
  | Ok
      (`Submission
        { Ptt.SSMTP.from; Ptt.SSMTP.recipients; Ptt.SSMTP.domain_from }) ->
      let romain_calascibetta =
        let open Mrmime.Mailbox in
        Local.[ w "romain"; w "calascibetta" ]
        @ Domain.(domain, [ a "gmail"; a "com" ]) in
      let anil =
        let open Mrmime.Mailbox in
        Local.[ w "anil" ] @ Domain.(domain, [ a "recoil"; a "org" ]) in
      let gmail =
        let open Mrmime.Mailbox in
        Domain.(v domain [ a "gmail"; a "com" ]) in
      Alcotest.(check reverse_path)
        "from" (fst from)
        ((Rresult.R.get_ok <.> Colombe_emile.to_reverse_path)
           romain_calascibetta) ;
      Alcotest.(check (list forward_path))
        "recipients" (List.map fst recipients)
        [ (Rresult.R.get_ok <.> Colombe_emile.to_forward_path) anil ] ;
      Alcotest.(check domain)
        "domain" domain_from
        ((Rresult.R.get_ok <.> Colombe_emile.to_domain) gmail) ;
      Alcotest.(check unit) "empty stream" (check ()) () ;
      Alcotest.(check pass) "submission" () ()
  | Ok `Quit -> Alcotest.fail "Unexpected quit"
  | Error (`Error err) ->
      Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.fail "Unexpected connection close"

let smtp_test_6 =
  Alcotest.test_case "SMTP (submission) 6" `Quick @@ fun () ->
  let rdwr, check =
    rdwr_from_flows
      [ "EHLO gmail.com"; "MAIL FROM:<romain.calascibetta@gmail.com>"; "QUIT" ]
      [
        "220 x25519.net";
        "250-x25519.net at your service, [127.0.0.1]";
        "250-8BITMIME";
        "250-SMTPUTF8";
        "250-SIZE 16777216";
        "250 AUTH PLAIN";
        "530 Authentication required, buddy!";
        "221 Bye, buddy!";
      ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain = x25519;
      ipv4 = Ipaddr.V4.localhost;
      tls = fake_tls_config;
      zone = Mrmime.Date.Zone.gmt;
      size = 16777216L;
    } in
  match
    run (Ptt.SSMTP.m_submission_init ctx info [ Ptt.Mechanism.PLAIN ]) rdwr ()
  with
  | Ok `Quit ->
      Alcotest.(check unit) "empty stream" (check ()) () ;
      Alcotest.(check pass) "quit" () ()
  | Ok (`Authentication _) -> Alcotest.failf "Unexpected authentication"
  | Ok (`Submission _) -> Alcotest.failf "Unexpected submission"
  | Error (`Error err) ->
      Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.failf "Unexpected connection close"

let smtp_test_7 =
  Alcotest.test_case "SMTP (submission) 7" `Quick @@ fun () ->
  let rdwr, check =
    rdwr_from_flows
      [ "EHLO gmail.com"; "AUTH PLAIN" ]
      [
        "220 x25519.net";
        "250-x25519.net at your service, [127.0.0.1]";
        "250-8BITMIME";
        "250-SMTPUTF8";
        "250-SIZE 16777216";
        "250 AUTH PLAIN";
      ] in
  let ctx = Colombe.State.Context.make () in
  let info =
    {
      Ptt.SSMTP.domain = x25519;
      ipv4 = Ipaddr.V4.localhost;
      tls = fake_tls_config;
      zone = Mrmime.Date.Zone.gmt;
      size = 16777216L;
    } in
  match
    run (Ptt.SSMTP.m_submission_init ctx info [ Ptt.Mechanism.PLAIN ]) rdwr ()
  with
  | Ok (`Authentication (v, m)) ->
      let gmail =
        let open Mrmime.Mailbox in
        Domain.(v domain [ a "gmail"; a "com" ]) in
      Alcotest.(check unit) "empty stream" (check ()) () ;
      Alcotest.(check mechanism) "mechanism" m Ptt.Mechanism.PLAIN ;
      Alcotest.(check domain)
        "domain" v
        ((Rresult.R.get_ok <.> Colombe_emile.to_domain) gmail) ;
      Alcotest.(check pass) "authentication" () ()
  | Ok `Quit | Ok (`Submission _) ->
      Alcotest.failf "Unexpected quit or submission"
  | Error (`Error err) ->
      Alcotest.failf "Unexpected protocol error: %a" Ptt.SSMTP.pp_error err
  | Error `Connection_close -> Alcotest.failf "Unexpected connection close"

module Random = struct
  type g = unit

  type +'a io = 'a

  let generate ?g:_ _ = ()
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

  type +'a io = 'a

  let gethostbyname () domain_name =
    match Hashtbl.find fake_dns_resolvers domain_name with
    | v -> Ok v
    | exception Not_found ->
        Rresult.R.error_msgf "%a not found" Domain_name.pp domain_name

  let getmxbyname () domain_name =
    let mxs =
      Dns.Rr_map.Mx_set.add
        { Dns.Mx.preference = 0; mail_exchange = domain_name }
        Dns.Rr_map.Mx_set.empty in
    Ok mxs

  let extension () _ _ = Rresult.R.error_msgf "Extension are not available"
end

module Flow_tuyau = (val Conduit_unix.impl protocol)

module Flow = struct
  type t = Flow_tuyau.flow

  include Flow_tuyau

  let flow endpoint =
    let sockaddr =
      match endpoint with
      | Unix.ADDR_UNIX _ -> endpoint
      | Unix.ADDR_INET (fake_inet_addr, _) ->
          let fake_inet_addr = Ipaddr_unix.of_inet_addr fake_inet_addr in
          Unix.ADDR_INET
            ( Unix.inet_addr_loopback,
              Hashtbl.find fake_smtp_servers fake_inet_addr ) in
    connect sockaddr

  let recv flow buf off len =
    let socket = Protocol.file_descr flow in
    let raw = Cstruct.of_bytes ~off ~len buf in
    Thread.wait_read socket ;
    let res = recv flow raw in
    match res with
    | Ok (`Input len) ->
        Cstruct.blit_to_bytes raw 0 buf off len ;
        len
    | Ok `End_of_flow -> 0
    | Error err -> Fmt.failwith "%a" Flow_tuyau.pp_error err

  let send flow buf off len =
    let socket = Protocol.file_descr flow in
    let raw = Cstruct.of_string ~off ~len buf in
    Thread.wait_write socket ;
    let res = send flow raw in
    match res with
    | Ok _ -> ()
    | Error err -> Fmt.failwith "%a" Flow_tuyau.pp_error err
end

let make_submission_smtp_server mutex initialized ~info ~port =
  let module SMTP =
    Ptt.Relay.Make (Unix_scheduler) (IO) (Flow) (Resolver) (Random)
  in
  let conf_server = SMTP.create ~info in
  let messaged = SMTP.messaged conf_server in
  let close = ref false in
  let smtp_submission_server conf_server =
    let open Rresult in
    let module Server = (val Conduit_unix.Service.impl service) in
    Conduit_unix.Service.init ~service
      {
        inet_addr = Ipaddr_unix.to_inet_addr (Ipaddr.V4 info.SMTP.ipv4);
        port;
        capacity = 0x1000;
      }
    >>= fun t ->
    let module Flow = (val Conduit_unix.impl protocol) in
    let handle flow =
      let rs0 = SMTP.accept flow () conf_server in
      let rs1 = Flow.close flow in
      match (rs0, rs1) with
      | Ok _, Ok _ -> ()
      | Error err, _ -> Fmt.epr "%a" SMTP.pp_error err
      | Ok _, Error err -> Fmt.epr "%a" Flow.pp_error err in
    let rec go () =
      let rd, _, _ = Thread.select [ (t :> Unix.file_descr) ] [] [] 0.1 in
      (if !close
      then
        let _ = Server.close t in
        Thread.exit ()) ;
      if List.length rd = 0
      then go ()
      else
        Server.accept t >>= fun flow ->
        (* XXX(dinosaure): we assert that [handle] finish in any cases. So we
           don't need to [join] it. *)
        let _ = Thread.create handle flow in
        Thread.yield () ;
        go () in
    Thread.delay 0.5 ;
    Fmt.epr ">>> Signal initialization.\n%!" ;
    Mutex.lock mutex ;
    Condition.signal initialized ;
    Mutex.unlock mutex ;
    Thread.yield () ;
    R.reword_error (R.msgf "%a" Server.pp_error) (go ()) in
  let smtp_logic messaged ms =
    let rec go () =
      if !close
      then (
        Fmt.epr ">>> Close logic server.\n%!" ;
        Thread.exit ()) ;
      let () = SMTP.Md.await messaged in
      match SMTP.Md.pop messaged with
      | Some (key, queue, _) ->
          SMTP.Md.close queue ;
          Queue.push key ms ;
          Thread.yield () ;
          go ()
      | None ->
          Thread.yield () ;
          go () in
    go () in
  let queue = Queue.create () in
  let server () =
    let _th1 = Thread.create smtp_submission_server conf_server in
    let _th2 = Thread.create (smtp_logic messaged) queue in
    let rec go () =
      if !close
      then (
        Fmt.epr ">>> Kill threads.\n%!"
        (* ; Thread.kill th1
           ; Thread.kill th2 *) ;
        queue
        (* XXX(dinosaure): it seems that [Thread.yield] does not properly
           re-schedule threads and this thread takes the opportunity to do an
           infinite loop (when [!close] will never be updated). It's why we put a
           [Thread.delay] to give a chance to other threads to be computed. *))
      else (
        Thread.delay 0.5 ;
        go ()) in
    go () in
  ( (fun () -> close := true),
    Thread.create server (),
    fun () -> Queue.fold (rev List.cons) [] queue )

let sendmail ipv4 port ~domain sender recipients contents =
  let stream = stream_of_string_list contents in
  let stream =
    (function
      | Some x -> Some (x ^ "\r\n", 0, String.length x + 2) | None -> None)
    <.> stream in
  let stream = Unix_scheduler.inj <.> stream in
  let tls_config =
    Tls.Config.client ~authenticator:(fun ~host:_ _ -> Ok None) () in
  let ctx = Sendmail_with_starttls.Context_with_tls.make () in
  let rdwr =
    {
      Colombe.Sigs.rd =
        (fun fd buf off len ->
          Thread.wait_read fd ;
          let res = Unix.read fd buf off len in
          Unix_scheduler.inj res);
      Colombe.Sigs.wr =
        (fun fd buf off len ->
          Thread.wait_write fd ;
          let _ = Unix.write fd (Bytes.unsafe_of_string buf) off len in
          Unix_scheduler.inj ());
    } in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect socket
    (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr (Ipaddr.V4 ipv4), port)) ;
  let res =
    Sendmail_with_starttls.sendmail unix rdwr socket ctx tls_config ~domain
      sender recipients stream in
  match Unix_scheduler.prj res with
  | Ok () -> Unix.close socket
  | Error err -> Fmt.failwith "%a" Sendmail_with_starttls.pp_error err

let key = Alcotest.testable Ptt.Messaged.pp Ptt.Messaged.equal

let full_test_0 =
  Alcotest.test_case "Receive one email from Anil" `Quick @@ fun () ->
  let romain_calascibetta =
    (Rresult.R.get_ok <.> Colombe_emile.to_forward_path)
      (let open Mrmime.Mailbox in
      Local.[ w "romain"; w "calascibetta" ]
      @ Domain.(domain, [ a "gmail"; a "com" ])) in
  let anil =
    (Rresult.R.get_ok <.> Colombe_emile.to_reverse_path)
      (let open Mrmime.Mailbox in
      Local.[ w "anil" ] @ Domain.(domain, [ a "recoil"; a "org" ])) in
  let mutex = Mutex.create () and initialized = Condition.create () in
  let close, th0, inbox =
    make_submission_smtp_server mutex initialized
      ~info:
        {
          Ptt.SMTP.domain = gmail;
          ipv4 = Ipaddr.V4.localhost;
          tls = fake_tls_config;
          zone = Mrmime.Date.Zone.GMT;
          size = 0x1000000000L;
        }
      ~port:8888 in
  let recoil = (Colombe.Domain.of_string_exn <.> Domain_name.to_string) recoil in
  let sendmail contents =
    Mutex.lock mutex ;
    Condition.wait initialized mutex ;
    Mutex.unlock mutex ;
    Thread.yield () ;
    sendmail Ipaddr.V4.localhost 8888 ~domain:recoil anil
      [ romain_calascibetta ] contents in
  let th1 =
    Thread.create sendmail
      [
        "From: anil@recoil.org";
        "Subject: SMTP server, PLZ!";
        "";
        "Hello World!";
      ] in
  Thread.join th1 ;
  close () ;
  Thread.join th0 ;
  let contents = inbox () in
  Alcotest.(check (list key))
    "inbox" contents
    [
      Ptt.Messaged.v ~domain_from:recoil ~from:(anil, [])
        ~recipients:[ (romain_calascibetta, []) ]
        0L;
    ]

let full_test_1 =
  Alcotest.test_case "Receive emails from Anil and Thomas" `Quick @@ fun () ->
  let romain_calascibetta =
    (Rresult.R.get_ok <.> Colombe_emile.to_forward_path)
      (let open Mrmime.Mailbox in
      Local.[ w "romain"; w "calascibetta" ]
      @ Domain.(domain, [ a "gmail"; a "com" ])) in
  let anil =
    (Rresult.R.get_ok <.> Colombe_emile.to_reverse_path)
      (let open Mrmime.Mailbox in
      Local.[ w "anil" ] @ Domain.(domain, [ a "recoil"; a "org" ])) in
  let thomas =
    (Rresult.R.get_ok <.> Colombe_emile.to_reverse_path)
      (let open Mrmime.Mailbox in
      Local.[ w "thomas" ] @ Domain.(domain, [ a "gazagnaire"; a "org" ])) in
  let mutex = Mutex.create () and initialized = Condition.create () in
  let close, th0, inbox =
    make_submission_smtp_server mutex initialized
      ~info:
        {
          Ptt.SMTP.domain = gmail;
          ipv4 = Ipaddr.V4.localhost;
          tls = fake_tls_config;
          zone = Mrmime.Date.Zone.GMT;
          size = 0x1000000000L;
        }
      ~port:8888 in
  let recoil = (Colombe.Domain.of_string_exn <.> Domain_name.to_string) recoil in
  let gazagnaire =
    (Colombe.Domain.of_string_exn <.> Domain_name.to_string) gazagnaire in
  let sendmail ?(waiting = true) ~domain sender contents =
    if waiting
    then (
      Mutex.lock mutex ;
      Condition.wait initialized mutex ;
      Mutex.unlock mutex ;
      Thread.yield ()) ;
    sendmail Ipaddr.V4.localhost 8888 ~domain sender [ romain_calascibetta ]
      contents in
  let th1 =
    Thread.create
      (sendmail ~domain:recoil anil)
      [
        "From: anil@recoil.org";
        "Subject: SMTP server, PLZ!";
        "";
        "Hello World!";
      ] in
  Thread.join th1 ;
  let th2 =
    Thread.create
      (sendmail ~waiting:false ~domain:gazagnaire thomas)
      [
        "From: anil@recoil.org";
        "Subject: SMTP server, PLZ!";
        "";
        "Hello World!";
      ] in
  Thread.join th2 ;
  close () ;
  Thread.join th0 ;
  let contents = inbox () in
  Alcotest.(check (list key))
    "inbox" contents
    [
      Ptt.Messaged.v ~domain_from:gazagnaire ~from:(thomas, [])
        ~recipients:[ (romain_calascibetta, []) ]
        1L;
      Ptt.Messaged.v ~domain_from:recoil ~from:(anil, [])
        ~recipients:[ (romain_calascibetta, []) ]
        0L;
    ]

let () =
  Alcotest.run "ptt"
    [
      ("mechanism", [ mechanism_test_0 ]);
      ("authentication", [ authentication_test_0 ]);
      ("aggregate", [ aggregate_test_0 ]);
      ("messaged", [ messaged_test_0; messaged_test_1 ]);
      ( "SMTP",
        [
          smtp_test_0;
          smtp_test_1;
          smtp_test_2;
          smtp_test_3;
          smtp_test_4;
          smtp_test_5;
          smtp_test_6;
          smtp_test_7;
        ] );
      ("Server", [ full_test_0; full_test_1 ]);
    ]
