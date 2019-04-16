module Unix_io = Dkim.Sigs.Make(struct type +'a t = 'a end)

let src = Logs.Src.create "ptt.dkim" ~doc:"logs DKIM event"
module Log = (val Logs.src_log src : Logs.LOG)

module Caml_flow = struct
  type backend = Unix_io.t
  type flow = in_channel

  let input flow buf off len = Unix_io.inj (input flow buf off len)
end

module Udns = struct
  include Udns_client_unix

  type t = Uflow.t
  type backend = Unix_io.t

  let getaddrinfo t `TXT domain_name =
    match getaddrinfo t Udns.Rr_map.Txt domain_name with
    | Ok (_ttl, txtset) -> Unix_io.inj (Ok (Udns.Rr_map.Txt_set.elements txtset))
    | Error _ as err -> Unix_io.inj err
end

let ( <.> ) f g = fun x -> f (g x)

let unix =
  { Dkim.Sigs.bind= (fun x f -> f (Unix_io.prj x))
  ; return= Unix_io.inj }

let string_of_raw x =
  let buf = Buffer.create 16 in
  List.iter
    (function
      | `CR n -> Buffer.add_string buf (String.make n '\r')
      | `LF n -> Buffer.add_string buf (String.make n '\n')
      | `WSP s -> Buffer.add_string buf s
      | `CRLF -> Buffer.add_string buf "\r\n"
      | `Text x -> Buffer.add_string buf x
      | `Encoded x ->
        let x = Mrmime.Encoded_word.reconstruct x in
        Buffer.add_string buf x)
    x ; Buffer.contents buf

let hxd_colorscheme =
  Hxd.O.colorscheme_of_array (Array.make 256 `None)

let unzip l =
  let rec go (ra, rb) = function
    | [] -> List.rev ra, List.rev rb
    | (a, b) :: r -> go (a :: ra, b :: rb) r in
  go ([], []) l

let verify ic new_line =
  let hxd = Hxd.O.xxd hxd_colorscheme in
  let newline = match new_line with `CRLF -> Dkim.CRLF | `LF -> Dkim.LF in
  match Dkim.extract_dkim ~newline ic unix (module Caml_flow) |> Unix_io.prj with
  | Error _ as err -> err
  | Ok extracted ->
    let dkim_fields = List.fold_left (fun a (field, raw, value) -> match Dkim.post_process_dkim value with
        | Ok value -> (field, raw, value) :: a
        | Error (`Msg err) ->
          let raw = string_of_raw raw in
          Log.err (fun m -> m "Invalid DKIM-field: @[<hov>%a@]." (Hxd_string.pp hxd) raw) ;
          Log.err (fun m -> m "Error: %s." err) ; a)
        [] extracted.Dkim.dkim_fields in

    let body = Dkim.extract_body ~newline ic unix (module Caml_flow) ~prelude:extracted.Dkim.prelude in
    let body = Unix_io.prj body in

    let dns = Udns.create () in

    let server_keys =
      List.map
        (fun (_, _, value) -> Unix_io.prj (Dkim.extract_server dns unix (module Udns) value))
        dkim_fields in

    let server_keys, dkim_fields =
      List.fold_left2 (fun a server_key ((_, _, x) as dkim_field) ->
          let open Rresult.R in match server_key >>= Dkim.post_process_server with
          | Ok server_key -> (server_key, dkim_field) :: a
          | Error (`Msg err) ->
            Log.err (fun m -> m "Retrieved an error from %s.%a: %s."
                        (Dkim.selector x)
                        Domain_name.pp (Dkim.domain x)
                        err) ;
            a)
        [] server_keys dkim_fields
      |> unzip in

    let ress =
      List.map2 (fun (raw_field_dkim, raw_dkim, dkim) server_key ->
          Dkim.domain dkim, Dkim.verify extracted.Dkim.fields (raw_field_dkim, raw_dkim) dkim server_key body)
        dkim_fields server_keys in

    Ok ress
