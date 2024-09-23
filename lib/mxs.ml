type key = Dns.Mx.t

let pp_key : key Fmt.t =
 fun ppf elt ->
  Fmt.pf ppf "{ @[<hov>preference= %d;@ mail_exchange= %a;@] }"
    elt.Dns.Mx.preference Domain_name.pp
    elt.Dns.Mx.mail_exchange

module Key = struct
  type t = key

  let compare {Dns.Mx.preference= a; _} {Dns.Mx.preference= b; _} = Int.compare a b
end

include (Map.Make (Key) : Map.S with type key := key)

let v ~preference ~domain:mail_exchange ipaddr =
  singleton { preference; mail_exchange } ipaddr

let vs =
  (Fun.flip List.fold_left empty) begin fun acc (mx, ipaddr) ->
    add mx ipaddr acc end
