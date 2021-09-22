open Rresult
open Lwt.Infix

let ( >>? ) = Lwt_result.bind

module DNS = Dns_mirage.Make (Tcpip_stack_socket.V4V6)

let ns_update ~now_d_ps ~g (ipaddr, port) ~dns_key stack ~domain spf =
  let key_name, key_zone, dns_key =
    let key_name, dns_key =
      R.failwith_error_msg (Dns.Dnskey.name_key_of_string dns_key) in
    match
      Domain_name.find_label key_name (function
        | "_update" -> true
        | _ -> false)
    with
    | None -> Fmt.failwith "The given DNS key is not an update key"
    | Some idx ->
      let amount = succ idx in
      let zone = Domain_name.(host_exn (drop_label_exn ~amount key_name)) in
      key_name, zone, dns_key in
  Tcpip_stack_socket.V4V6.TCP.create_connection stack (ipaddr, port)
  >|= R.reword_error (R.msgf "%a" Tcpip_stack_socket.V4V6.TCP.pp_error)
  >>? fun flow ->
  let str = Spf.record_to_string spf in
  (* TODO(dinosaure): split to 80 characters? *)
  let v =
    Dns.Packet.Update.Add Dns.Rr_map.(B (Txt, (3600l, Txt_set.singleton str)))
  in
  let packet =
    let header =
      ( Randomconv.int16 (Mirage_crypto_rng.Fortuna.generate ~g)
      , Dns.Packet.Flags.empty ) in
    let zone = Dns.Packet.Question.create key_zone Dns.Rr_map.Soa in
    Dns.Packet.create header zone
      (`Update Domain_name.Map.(empty, singleton domain [v])) in
  Lwt.bind
    ( Dns_tsig.encode_and_sign ~proto:`Tcp packet
        (Ptime.v (now_d_ps ()))
        dns_key key_name
    |> R.reword_error (R.msgf "%a" Dns_tsig.pp_s)
    |> Lwt.return
    >>? fun (data, mac) ->
      DNS.send_tcp flow data
      >|= R.reword_error (fun _ ->
              R.msgf "Impossible to send a DNS packet to %a:%d" Ipaddr.pp ipaddr
                port)
      >>? fun () ->
      DNS.read_tcp (DNS.of_flow flow)
      >|= R.reword_error (fun _ ->
              R.msgf "Impossible to read a DNS packet from %a:%d" Ipaddr.pp
                ipaddr port)
      >>? fun data ->
      Dns_tsig.decode_and_verify
        (Ptime.v (now_d_ps ()))
        dns_key key_name ~mac data
      |> R.reword_error (R.msgf "%a" Dns_tsig.pp_e)
      |> Lwt.return
      >>? fun (packet', _tsig, _mac) ->
      match Dns.Packet.reply_matches_request ~request:packet packet' with
      | Ok _ -> Lwt.return_ok ()
      | Error err -> Lwt.return_error (R.msgf "%a" Dns.Packet.pp_mismatch err)
    )
  @@ fun res ->
  Tcpip_stack_socket.V4V6.TCP.close flow >>= fun () -> Lwt.return res
