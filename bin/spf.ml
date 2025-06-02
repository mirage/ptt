open Rresult
open Lwt.Infix

let ( >>? ) = Lwt_result.bind

module DNS = Dns_mirage.Make (Tcpip_stack_socket.V4V6)

let ns_check ~domain spf =
  let module DNS = struct
    type t = Dns_client_lwt.t

    type error =
      [ `Msg of string
      | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
      | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ]

    let getrrecord dns key domain_name =
      Dns_client_lwt.get_resource_record dns key domain_name
  end in
  let he = Happy_eyeballs_lwt.create () in
  let _dns = Dns_client_lwt.create he in
  assert false
(* Uspf_lwt.get ~domain dns (module DNS) >>= function
  | Ok spf' when Uspf.Term.equal spf spf' -> Lwt.return `Already_registered
  | Ok _ -> Lwt.return `Must_be_updated
  | _ -> Lwt.return `Not_found *)

let ns_update (ipaddr, port) ~dns_key stack ~domain spf =
  ns_check ~domain spf >>= function
  | `Already_registered -> Lwt.return_ok ()
  | `Must_be_updated | `Not_found ->
    let key_name, key_zone, dns_key =
      let key_name, dns_key = dns_key in
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
    let str = Uspf.Term.to_string spf in
    (* TODO(dinosaure): split to 80 characters? *)
    let v =
      Dns.Packet.Update.Add Dns.Rr_map.(B (Txt, (3600l, Txt_set.singleton str)))
    in
    let packet =
      let header =
        Randomconv.int16 Mirage_crypto_rng.generate, Dns.Packet.Flags.empty
      in
      let zone = Dns.Packet.Question.create key_zone Dns.Rr_map.Soa in
      Dns.Packet.create header zone
        (`Update Domain_name.Map.(empty, singleton domain [v])) in
    Lwt.bind
      ( Dns_tsig.encode_and_sign ~proto:`Tcp packet (Ptime_clock.now ()) dns_key
          key_name
      |> R.reword_error (R.msgf "%a" Dns_tsig.pp_s)
      |> Lwt.return
      >>? fun (data, mac) ->
        DNS.send_tcp flow (Cstruct.of_string data)
        >|= R.reword_error (fun _ ->
                R.msgf "Impossible to send a DNS packet to %a:%d" Ipaddr.pp
                  ipaddr port)
        >>? fun () ->
        DNS.read_tcp (DNS.of_flow flow)
        >|= R.reword_error (fun _ ->
                R.msgf "Impossible to read a DNS packet from %a:%d" Ipaddr.pp
                  ipaddr port)
        >>? fun data ->
        Dns_tsig.decode_and_verify (Ptime_clock.now ()) dns_key key_name ~mac
          (Cstruct.to_string data)
        |> R.reword_error (R.msgf "%a" Dns_tsig.pp_e)
        |> Lwt.return
        >>? fun (packet', _tsig, _mac) ->
        match Dns.Packet.reply_matches_request ~request:packet packet' with
        | Ok _ -> Lwt.return_ok ()
        | Error err -> Lwt.return_error (R.msgf "%a" Dns.Packet.pp_mismatch err)
      )
    @@ fun res ->
    Tcpip_stack_socket.V4V6.TCP.close flow >>= fun () -> Lwt.return res

let run (ipaddr, port) dns_key domain spf =
  let open Tcpip_stack_socket.V4V6 in
  TCP.connect ~ipv4_only:false ~ipv6_only:false Ipaddr.V4.Prefix.global None
  >>= fun stack -> ns_update (ipaddr, port) ~dns_key stack ~domain spf

let run (ipaddr, port) dns_key domain spf =
  match Lwt_main.run (run (ipaddr, port) dns_key domain spf) with
  | Ok () -> `Ok 0
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

open Cmdliner

let address =
  let parser str =
    match String.split_on_char ':' str with
    | [ipaddr] -> R.(Ipaddr.of_string ipaddr >>= fun ipaddr -> ok (ipaddr, 53))
    | [ipaddr; port] -> (
      try
        R.(
          Ipaddr.of_string ipaddr >>= fun ipaddr ->
          ok (ipaddr, int_of_string port))
      with _ -> R.error_msgf "Invalid port: %S" port)
    | _ -> R.error_msgf "Invalid IP address: %S" str in
  let pp ppf (ipaddr, port) = Fmt.pf ppf "%a:%d" Ipaddr.pp ipaddr port in
  Arg.conv (parser, pp) ~docv:"<address>"

let dns_key =
  let parser = Dns.Dnskey.name_key_of_string in
  let pp ppf (domain_name, dns_key) =
    Fmt.pf ppf "%a:%a" Domain_name.pp domain_name Dns.Dnskey.pp dns_key in
  Arg.conv (parser, pp) ~docv:"<dns-key>"

let domain =
  let parser = Domain_name.of_string in
  let pp = Domain_name.pp in
  Arg.conv (parser, pp) ~docv:"<domain>"

let spf =
  let parser = Uspf.Term.parse_record in
  let pp = Uspf.Term.pp in
  Arg.conv (parser, pp) ~docv:"<spf-record>"

let address =
  let doc = "Address of the primary DNS server." in
  Arg.(required & pos 0 (some address) None & info [] ~doc ~docv:"<address>")

let dns_key =
  let doc = "DNS key used to be able to update the primary DNS server." in
  Arg.(required & pos 1 (some dns_key) None & info [] ~doc ~docv:"<dns-key>")

let domain =
  let doc = "Domain where the SMTP service is reachable." in
  Arg.(required & pos 2 (some domain) None & info [] ~doc ~docv:"<domain>")

let spf =
  let doc = "The SPF description." in
  Arg.(required & pos 3 (some spf) None & info [] ~doc ~docv:"<spf-record>")

let term = Term.(ret (const run $ address $ dns_key $ domain $ spf))

let cmd =
  let doc = "A tool to update the zone of a given domain with SPF metadata." in
  let man = [] in
  Cmd.v (Cmd.info "spf" ~doc ~man) term

let () = exit @@ Cmd.eval' cmd
