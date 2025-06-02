module Make (Destination : sig
  val ipaddr : Ipaddr.t
end) : sig
  include
    Dns_client_mirage.S
      with type 'a Transport.io = 'a Lwt.t
       and type Transport.stack = unit
end
