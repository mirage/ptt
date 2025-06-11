let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt

module Make (Destination : sig
  val ipaddr : Ipaddr.t
end) =
struct
  module Transport = struct
    type context = unit
    type stack = unit
    type t = unit
    type 'a io = 'a Lwt.t

    type io_addr =
      [ `Plaintext of Ipaddr.t * int
      | `Tls of Tls.Config.client * Ipaddr.t * int ]

    let create ?nameservers:_ ~timeout:_ _ = ()
    let nameservers _ = `Tcp, []
    let rng _ = String.empty
    let clock _ = 0L

    let connect _ =
      Lwt.return_error (msgf "Ptt_fake_dns.Transport.connect: not implemented")

    let send_recv _ _ =
      Lwt.return_error
        (msgf "Ptt_fake_dns.Transport.send_Recv: not implemented")

    let close _ = Lwt.return_unit
    let bind = Lwt.bind
    let lift = Lwt.return
    let happy_eyeballs _ = assert false
  end

  type happy_eyeballs = unit

  include Dns_client.Make (Transport)

  (* NOTE(dinosaure): [ptt] only uses [getaddrinfo], [gethostbyname] &
     [gethostbyname6]. The rest is useless. *)

  let getaddrinfo : type a.
         t
      -> a Dns.Rr_map.key
      -> 'x Domain_name.t
      -> (a, [> `Msg of string ]) result Lwt.t =
   fun _ record domain_name ->
    match record, Domain_name.host domain_name with
    | Dns.Rr_map.Mx, Ok mail_exchange ->
      Lwt.return_ok
        (0l, Dns.Rr_map.Mx_set.singleton {Dns.Mx.preference= 0; mail_exchange})
    | _ ->
      Lwt.return_error
        (msgf "Impossible to get %a from %a" Dns.Rr_map.ppk
           Dns.Rr_map.(K record)
           Domain_name.pp domain_name)

  let gethostbyname _t domain_name =
    match Destination.ipaddr with
    | Ipaddr.V4 ipv4 -> Lwt.return_ok ipv4
    | _ -> Lwt.return_error (msgf "%a not found" Domain_name.pp domain_name)

  let gethostbyname6 _t domain_name =
    match Destination.ipaddr with
    | Ipaddr.V6 ipv6 -> Lwt.return_ok ipv6
    | _ -> Lwt.return_error (msgf "%a not found" Domain_name.pp domain_name)

  let get_resource_record : type a.
         t
      -> a Dns.Rr_map.key
      -> 'x Domain_name.t
      -> ( a
         , [> `Msg of string
           | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
           | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ] )
         result
         Lwt.t =
   fun _t record domain_name ->
    match record, Domain_name.host domain_name with
    | Dns.Rr_map.Mx, Ok mail_exchange ->
      Lwt.return_ok
        (0l, Dns.Rr_map.Mx_set.singleton {Dns.Mx.preference= 0; mail_exchange})
    | _ ->
      Lwt.return_error
        (msgf "Impossible to get %a from %a" Dns.Rr_map.ppk
           Dns.Rr_map.(K record)
           Domain_name.pp domain_name)

  let get_raw_reply : type a.
         t
      -> a Dns.Rr_map.key
      -> 'x Domain_name.t
      -> (Dns.Packet.reply, [> `Msg of string | `Partial ]) result Lwt.t =
   fun _t _record _domain_name ->
    Lwt.return_error
      (msgf "Impossible to get %a from %a" Dns.Rr_map.ppk
         Dns.Rr_map.(K _record)
         Domain_name.pp _domain_name)

  let nameserver_of_string _ =
    Error (msgf "Ptt_fake_dns.nameserver_of_string: not implemented")

  let nameservers _ = `Tcp, []
  let transport _ = ()

  let connect ?cache_size ?edns ?nameservers:_ ?timeout () =
    create ?cache_size ?edns ~nameservers:(`Tcp, []) ?timeout () |> Lwt.return
end
