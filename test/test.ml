module Unix_scheduler = Colombe.Sigs.Make(struct type +'a t = 'a end)

let unix =
  let open Unix_scheduler in
  { Colombe.Sigs.bind= (fun x f -> f (prj x))
  ; Colombe.Sigs.return= (fun x -> inj x) }

let mechanism = Alcotest.testable Ptt.Mechanism.pp Ptt.Mechanism.equal
let msg = Alcotest.testable Rresult.R.pp_msg (fun (`Msg a) (`Msg b) -> String.equal a b)

let mechanism_test_0 =
  Alcotest.test_case "mechanism 0" `Quick @@ fun () ->
  Alcotest.(check mechanism) "plain" (Ptt.Mechanism.of_string_exn "plain") Ptt.Mechanism.PLAIN ;
  Alcotest.(check mechanism) "PLAIN" (Ptt.Mechanism.of_string_exn "PLAIN") Ptt.Mechanism.PLAIN ;
  Alcotest.(check mechanism) "PlAiN" (Ptt.Mechanism.of_string_exn "PlAiN") Ptt.Mechanism.PLAIN ;
  Alcotest.check_raises "PLAIZ" (Invalid_argument "Invalid mechanism: PLAIZ") (fun () -> ignore @@ Ptt.Mechanism.of_string_exn "PLAIZ") ;
;;

let auth0 =
  let module Map = Map.Make(struct type t = Emile.local let compare = Emile.compare_local ~case_sensitive:false end) in
  let open Mrmime.Mailbox in
  let m =
    Map.empty
    |> Map.add Local.(v [ w "romain"; w "calascibetta" ]) Digestif.(digest_string SHA1 "toto")
    |> Map.add Local.(v [ w "thomas" ]) Digestif.(digest_string SHA1 "tata")
    |> Map.add Local.(v [ w "anil" ]) Digestif.(digest_string SHA1 "tutu")
    |> Map.add Local.(v [ w "hannes" ]) Digestif.(digest_string SHA1 "titi")
    |> Map.add Local.(v [ w "gemma" ]) Digestif.(digest_string SHA1 "") in
  let f username password =
    match Map.find username m with
    | v -> Unix_scheduler.inj (Digestif.(equal SHA1 password v))
    | exception Not_found -> Unix_scheduler.inj false in
  Ptt.Authentication.v f

let authentication_test_0 =
  Alcotest.test_case "authentication 0" `Quick @@ fun () ->
  let auth hash mechanism authenticator fmt =
    Fmt.kstrf
      (fun payload ->
         Ptt.Authentication.decode_authentication
           unix hash mechanism authenticator (Base64.encode_exn payload)
         |> Unix_scheduler.prj)
      fmt in
  let plain_none = Ptt.Authentication.PLAIN None in
  Alcotest.(check (result bool msg)) "romain" (Ok true)
    (auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "romain.calascibetta" "toto") ;
  Alcotest.(check (result bool msg)) "thomas" (Ok true)
    (auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "thomas" "tata") ;
  Alcotest.(check (result bool msg)) "anil" (Ok true)
    (auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "anil" "tutu") ;
  Alcotest.(check (result bool msg)) "hannes" (Ok true)
    (auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "hannes" "titi") ;
  Alcotest.(check (result bool msg)) "gemma" (Ok true)
    (auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "gemma" "") ;
  Alcotest.(check (result bool msg)) "romain (wrong)" (Ok false)
    (auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "romain.calascibetta" "titi") ;
  Alcotest.(check (result bool msg)) "pierre" (Ok false)
    (auth Digestif.SHA1 plain_none auth0 "\000%s\000%s" "pierre.caillou" "toto") ;
  Alcotest.(check (result bool msg)) "bad stamp" (Error (`Msg "Unexpected stamp"))
    (auth Digestif.SHA1 plain_none auth0 "stamp\000%s\000%s" "romain.calascibetta" "toto") ;
  Alcotest.(check (result bool msg)) "malformed" (Error (`Msg "Invalid input"))
    (auth Digestif.SHA1 plain_none auth0 "salut les copains") ;
  Alcotest.(check (result bool msg)) "no stamp" (Error (`Msg "Invalid stamp"))
    (auth Digestif.SHA1 (Ptt.Authentication.PLAIN (Some "stamp")) auth0 "\000%s\000%s" "anil" "tutu") ;
  Alcotest.(check (result bool msg)) "invalid username" (Error (`Msg "Invalid username: \"\""))
    (auth Digestif.SHA1 plain_none auth0 "\000\000%s" "tutu") ;
;;

let () =
  Alcotest.run "ptt"
    [ "mechanism", [ mechanism_test_0 ]
    ; "authentication", [ authentication_test_0 ]]
