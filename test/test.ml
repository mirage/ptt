module Unix_scheduler = Colombe.Sigs.Make(struct type +'a t = 'a end)

let unix =
  let open Unix_scheduler in
  { Colombe.Sigs.bind= (fun x f -> f (prj x))
  ; Colombe.Sigs.return= (fun x -> inj x) }

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

let ( <.> ) f g = fun x -> f (g x)

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

let x25519 = Domain_name.(host_exn <.> of_string_exn) "x25519.net"
let gmail = Domain_name.(host_exn <.> of_string_exn) "gmail.com"
let recoil = Domain_name.(host_exn <.> of_string_exn) "recoil.org"
let nqsb = Domain_name.(host_exn <.> of_string_exn) "nqsb.io"
let gazagnaire = Domain_name.(host_exn <.> of_string_exn) "gazagnaire.org"

let pp_unresolved ppf = function
  | `All -> Fmt.string ppf "<all>"
  | `Postmaster -> Fmt.string ppf "<postmaster>"
  | `Local vs -> Fmt.Dump.list Emile.pp_local ppf vs

let equal_unresolved a b = match a, b with
  | `All, `All -> true
  | `Postmaster, `Postmaster -> true
  | `Local a, `Local b ->
    let a = List.sort (Emile.compare_local ~case_sensitive:true) a in
    let b = List.sort (Emile.compare_local ~case_sensitive:true) b in
    ( try List.for_all2 (Emile.equal_local ~case_sensitive:true) a b
      with _ -> false )
  | _, _ -> false

let unresolved = Alcotest.testable pp_unresolved equal_unresolved

let aggregate_test_0 =
  Alcotest.test_case "aggregate 0" `Quick @@ fun () ->
  let open Mrmime.Mailbox in
  let m0 = Local.[ w "romain"; w "calascibetta" ] @ Domain.(domain, [ a "gmail"; a "com" ]) in
  let m1 = Local.[ w "thomas" ] @ Domain.(domain, [ a "gazagnaire"; a "org" ]) in
  let m2 = Local.[ w "anil" ] @ Domain.(domain, [ a "recoil"; a "org" ]) in
  let m3 = Local.[ w "gemma"; w "d"; w "gordon" ] @ Domain.(domain, [ a "gmail"; a "com" ]) in
  let ms = List.map (Rresult.R.get_ok <.> Colombe_emile.to_forward_path) [ m0; m1; m2; m3; ] in
  let ms = Colombe.Forward_path.Domain ((Rresult.R.get_ok <.> Colombe_emile.to_domain) Domain.(v domain [ a "nqsb"; a "io" ])) :: ms in
  let u, r = Ptt.Aggregate.aggregate_by_domains ~domain:x25519 ms in
  Alcotest.(check bool) "resolved is empty" true (Ptt.Aggregate.By_ipaddr.is_empty r) ;
  Alcotest.(check unresolved) "unresolved gmail.com" (`Local [ Local.(v [ w "romain"; w "calascibetta" ])
                                                             ; Local.(v [ w "gemma"; w "d"; w "gordon" ])])
    (Ptt.Aggregate.By_domain.find gmail u) ;
  Alcotest.(check unresolved) "unresolved recoil.org" (`Local [ Local.(v [ w "anil" ])])
    (Ptt.Aggregate.By_domain.find recoil u) ;
  Alcotest.(check unresolved) "unresolved gazagnaire.org" (`Local [ Local.(v [ w "thomas" ])])
    (Ptt.Aggregate.By_domain.find gazagnaire u) ;
  Alcotest.(check unresolved) "unresolved nqsb.io" `All (Ptt.Aggregate.By_domain.find nqsb u) ;
;;

module Md = Ptt.Messaged.Make(Unix_scheduler)(IO)

let stream_of_string_list l =
  let l = ref l in
  let stream () = match !l with
    | [] -> None
    | x :: r ->
      l := r ; Some x in
  stream

let hello_world = stream_of_string_list ["Hello", 0, 5; " ", 0, 1; "World!", 0, 6]
let hello_buddy = stream_of_string_list ["Hello", 0, 5; " ", 0, 1; "buddy!", 0, 6]
let hello_guy = stream_of_string_list ["Hello", 0, 5; " ", 0, 1; "guy!", 0, 4]

let messaged_test_0 =
  Alcotest.test_case "messaged 0" `Quick @@ fun () ->
  let md = Md.create () in
  let do0 ~domain_from ~from v () =
    let key =
      Ptt.Messaged.v
        ~domain_from:((Rresult.R.get_ok <.> Colombe_emile.to_domain) domain_from)
        ~from:((Rresult.R.get_ok <.> Colombe_emile.to_reverse_path) from, [])
        ~recipients:[] 0L in
    let producer = Md.push md key in
    let rec consume () =
      match v () with
      | Some chunk -> producer (Some chunk) ; consume ()
      | None -> producer None in
    consume () in
  let contents = ref "" in
  let do1 () =
    Md.await md ; (* XXX(dinosaure): schedule [do1] __after__ [do0]. *)
    match Md.pop md with
    | Some (_, _, v) ->
      let buf = Buffer.create 0x100 in
      let rec consume () = match v () with
        | Some (str, off, len) ->
          Buffer.add_substring buf str off len ;
          consume ()
        | None -> contents := Buffer.contents buf in
      consume ()
    | None -> assert false in
  let domain_from = Mrmime.Mailbox.Domain.(v domain [ a "x25519"; a "net" ]) in
  let from =
    let open Mrmime.Mailbox in
    Local.[ w "romain"; w "calascibetta" ] @ Domain.(domain, [ a "gmail"; a "com" ]) in

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
  Alcotest.(check string) "(consumer & producer) payload" !contents "Hello buddy!" ;
  let th0 = Thread.create (do0 ~domain_from ~from hello_guy) () in
  Unix.sleep 1 ;
  let th1 = Thread.create do1 () in
  Thread.join th0 ;
  Thread.join th1 ;
  Alcotest.(check string) "(producer & consumer) payload" !contents "Hello guy!" ;
;;

let messaged_test_1 =
  Alcotest.test_case "messaged 1" `Quick @@ fun () ->
  let md = Md.create () in
  let last = ref 0 in
  let do0 ~domain_from ~from v () =
    last := 0 ;
    let key =
      Ptt.Messaged.v
        ~domain_from:((Rresult.R.get_ok <.> Colombe_emile.to_domain) domain_from)
        ~from:((Rresult.R.get_ok <.> Colombe_emile.to_reverse_path) from, [])
        ~recipients:[] 0L in
    let producer = Md.push md key in
    let rec consume () =
      match v () with
      | Some chunk -> producer (Some chunk) ; consume ()
      | None -> producer None in
    consume () in
  let do1 () =
    last := 1 ;
    Md.await md ; (* XXX(dinosaure): schedule [do1] __after__ [do0]. *)
    match Md.pop md with
    | Some (_, q, _) -> Md.close q
    | None -> assert false in
  let domain_from = Mrmime.Mailbox.Domain.(v domain [ a "x25519"; a "net" ]) in
  let from =
    let open Mrmime.Mailbox in
    Local.[ w "romain"; w "calascibetta" ] @ Domain.(domain, [ a "gmail"; a "com" ]) in

  let th0 = Thread.create (do0 ~domain_from ~from hello_world) () in
  let th1 = Thread.create do1 () in
  Thread.join th0 ;
  Thread.join th1 ;
  Alcotest.(check pass) "random schedule" () () ;
  let th1 = Thread.create do1 () in
  Unix.sleep 1 ;
  let th0 = Thread.create (do0 ~domain_from ~from hello_buddy) () in
  Thread.join th0 ;
  Thread.join th1 ;
  Alcotest.(check int) "(consumer & producer)" !last 0 ;
  let th0 = Thread.create (do0 ~domain_from ~from hello_guy) () in
  Unix.sleep 1 ;
  let th1 = Thread.create do1 () in
  Thread.join th0 ;
  Thread.join th1 ;
  Alcotest.(check int) "(producer & consumer)" !last 1 ;
;;

let () =
  Alcotest.run "ptt"
    [ "mechanism", [ mechanism_test_0 ]
    ; "authentication", [ authentication_test_0 ]
    ; "aggregate", [ aggregate_test_0 ]
    ; "messaged", [ messaged_test_0
                  ; messaged_test_1 ] ]
