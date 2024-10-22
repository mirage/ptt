(** The purpose of this module is to launch a ‘job’ which will send the emails
    it obtains (and which are transmitted by the push function returned by
    {!val:v}).

    To do this, we need a DNS resolver (to resolve the [MX] field of a domain),
    server information (so that it can identify itself to other SMTP servers),
    a TLS client configuration (to initiate encrypted communication with
    STARTTLS) and a happy-eyeballs instance to initiate a TCP/IP connection
    with the SMTP servers.

    An email that you want to send is described in such a way that it contains
    the sender, the recipients, the content, a unique ID and an error
    management policy.

    It can happen that the email cannot be sent (unavailable SMTP server,
    non-existent domain name, etc.). In this specific case, we need an error
    management policy so that we know if we need to send an error message back
    to the [Return-Path] announced by the email.

    Receivers must be aggregated according to the domain name and/or IP
    corresponding to the email exchange server. The idea is to send one email
    per domain listed in the receivers (and not one email per receiver).
*)

type recipients =
  { domain : [ `Ipaddr of Ipaddr.t | `Domain of [ `host ] Domain_name.t ]
  ; locals : [ `All | `Postmaster | `Some of Emile.local list ] }
and 'dns t and elt =
  { sender : Colombe.Reverse_path.t
  ; recipients : recipients
  ; data : string Lwt_stream.t
  ; policies : policy list
  ; id : Mrmime.MessageID.t }
and pool = { pool : 'a. (resource -> 'a Lwt.t) -> 'a Lwt.t }
and resource = bytes * bytes * (char, Bigarray.int8_unsigned_elt) Ke.Rke.t
and 'a push = 'a option -> unit
and policy = [ `Ignore ]

val pp_recipients : recipients Fmt.t

module Make
  (Clock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
  (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow) : sig
  val v :
       resolver:'dns Ptt_common.resolver
    -> ?pool:pool
    -> info:Ptt_common.info
    -> Tls.Config.client
    -> 'dns t * elt push

  val job : 'dns -> Happy_eyeballs.t -> 'dns t -> unit Lwt.t
end
