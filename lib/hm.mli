module Make
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Resolver : Ptt.Sigs.RESOLVER with type +'a io = 'a Lwt.t)
    (Stack : Tcpip.Stack.V4V6) : sig
  type dns

  val create :
       ?cache_size:int
    -> ?edns:[ `Auto | `Manual of Dns.Edns.t | `None ]
    -> ?nameservers:
         Dns.proto
         * [ `Plaintext of Ipaddr.t * int
           | `Tls of Tls.Config.client * Ipaddr.t * int ]
           list
    -> ?timeout:int64
    -> Stack.t
    -> dns

  val fiber :
       ?limit:int
    -> ?stop:Lwt_switch.t
    -> port:int
    -> tls:Tls.Config.client
    -> Stack.TCP.t
    -> Resolver.t
    -> Ptt.Logic.info
    -> Ptt.Relay_map.t
    -> dns
    -> unit Lwt.t
end
