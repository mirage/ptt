open Rresult

module Make (Stack : Tcpip.Stack.V4V6) : sig
  type 'k server

  type info = SSMTP.info = {
      domain: Colombe.Domain.t
    ; ipaddr: Ipaddr.t
    ; tls: Tls.Config.server option
    ; zone: Mrmime.Date.Zone.t
    ; size: int64
  }

  val info : 'k server -> info

  type error

  val pp_error : error Fmt.t

  val create :
       info:info
    -> authenticator:'k Authentication.t
    -> Mechanism.t list
    -> 'k server
       * (Msgd.key * string Lwt_stream.t * Msgd.result Lwt.u) Lwt_stream.t
       * (unit -> unit)

  val accept_without_starttls :
       ?encoder:(unit -> bytes)
    -> ?decoder:(unit -> bytes)
    -> ipaddr:Ipaddr.t
    -> Stack.TCP.flow
    -> 'dns
    -> 'dns Ptt_common.resolver
    -> Mirage_crypto_rng.g option
    -> 'k Digestif.hash
    -> 'k server
    -> (unit, error) result Lwt.t
  (** [accept flow resolver random alg server] is a simple SMTP process which
      accepts an incoming email iff the client is authentified. The method to
      safely check the password uses the hash algorithm [alg] and private
      information from the given [server] (see {!create}). If the user is
      correctly authentified, the incoming email is added into the internal
      [server]'s queue.

      The incoming email is accepted only if recipients given by the client are
      reachable {i via} the given [resolver] - see
      {!Common.Make.recipients_are_reachable}. Otherwise, the incoming email is
      discarded! *)
end
