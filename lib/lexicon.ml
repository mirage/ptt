let pp_recipients ppf { Sendmail.domain; locals; } =
  let pp_domain ppf = function
    | `Ipaddr (Ipaddr.V4 v4) -> Fmt.pf ppf "[%a]" Ipaddr.V4.pp v4
    | `Ipaddr (Ipaddr.V6 v6) -> Fmt.pf ppf "[IPv6:%a]" Ipaddr.V6.pp v6
    | `Domain domain -> Domain_name.pp ppf domain in
  let pp_local ppf local =
    let pp_elt ppf = function
      | `Atom x -> Fmt.string ppf x
      | `String x -> Fmt.pf ppf "%S" x in
    Fmt.(list ~sep:(any ".") pp_elt) ppf local in
  match locals with
  | `All -> Fmt.pf ppf "- <%a>\n" pp_domain domain
  | `Some locals ->
    let pp_elt local = Fmt.pf ppf "- %a@%a\n" pp_local local pp_domain domain in
    List.iter pp_elt locals

let impossible_to_send_an_email_to ~recipients mxs =
  Fmt.str {text|It's impossible to send an email to:
  %a

  We tried to send the email %a at %a to:
  %a

  All of them are unavailable.|text}
