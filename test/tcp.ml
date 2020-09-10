module Conduit_unix =
  Conduit.Make
    (struct type 'a t = 'a let bind x f = f x let return x = x end)
    (Cstruct)(Cstruct)

type endpoint = Unix.sockaddr

type protocol =
  { socket : Unix.file_descr
  ; sockaddr : Unix.sockaddr
  ; linger : Bytes.t
  ; mutable closed : bool }

let socket : protocol -> Unix.file_descr = fun { socket; _ } -> socket

module Protocol = struct
  type input = Cstruct.t
  type output = Cstruct.t
  type +'a io = 'a

  type error = Socket_closed

  let pp_error ppf Socket_closed = Fmt.string ppf "Socket closed"

  type nonrec endpoint = endpoint

  type flow = protocol =
    { socket : Unix.file_descr
    ; sockaddr : Unix.sockaddr
    ; linger : Bytes.t
    ; mutable closed : bool }

  let file_descr { socket; _ } = socket

  let connect sockaddr =
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let linger = Bytes.create 0x1000 in
    Unix.connect socket sockaddr ; Ok { socket; linger; sockaddr; closed= false; }

  let rec recv ({ socket; closed; _ } as t) raw =
    if closed
    then Ok `End_of_flow
      (* XXX(dinosaure): it's __not__ clear if we should return an error [Socket_closed]
         or just say that we reach end-of-input. From my perspective, only [send] should
         return an error in this case when we assume that the underlying [socket] is still
         open. In this view, [recv] has the ability to inform that the underlying [socket]
         is close to the user (eg. [recv flow Cstruct.empty = `End_of_input => socket is closed]).

         However, a such assumption is not clear for anybody. *)
    else
      try
        let max = Cstruct.len raw in
        let len = Unix.read socket t.linger 0 (min max (Bytes.length t.linger)) in
        if len = 0 then Ok `End_of_flow
        else ( Cstruct.blit_from_bytes t.linger 0 raw 0 len
             ; if len = Bytes.length t.linger && max > Bytes.length t.linger
               then
                 (* XXX(dinosaure): [raw] can be bigger than [t.linger]. In this case,
                    we should check if [socket] has pending data and fill the rest of [raw] then.
                    [Unix.select] should not wait with [0.]. *)
                 let (rd, _, _) = Unix.select [ socket ] [] [] 0. in
                 if List.length rd = 0
                 then Ok (`Input len)
                 else
                   let ( >>= ) x f = match x with
                     | Ok x -> f x
                     | Error err -> Error err in
                   recv t (Cstruct.shift raw len) >>= function
                   | `End_of_flow -> t.closed <- true ; Ok (`Input len)
                   | `Input rest -> Ok (`Input (len + rest))
               else Ok (`Input len) )
      with Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> recv t raw
         | Unix.(Unix_error (EINTR, _, _)) -> recv t raw

  let rec send ({ socket; closed; _ } as t) raw =
    if closed
    then Error Socket_closed
    else
      let max = Cstruct.len raw in
      let len0 = min (Bytes.length t.linger) max in
      Cstruct.blit_to_bytes raw 0 t.linger 0 len0 ;
      try let len1 = Unix.write socket t.linger 0 len0 in
        if len1 = len0
        then if max > len0
          then send t (Cstruct.sub raw len0 (max - len0))
          else Ok max
        else Ok len1
      with Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> send t raw
         | Unix.(Unix_error (EINTR, _, _)) -> send t raw
         | Unix.(Unix_error (EPIPE, _, _)) ->
           t.closed <- true ; Error Socket_closed

  let rec close t =
    try
      if not t.closed then Unix.close t.socket ;
      t.closed <- true ;
      Ok ()
    with Unix.(Unix_error (EINTR, _, _)) -> close t
end

let protocol = Conduit_unix.register ~protocol:(module Protocol)

type configuration =
  { inet_addr : Unix.inet_addr
  ; port : int
  ; capacity : int }

type service = Unix.file_descr

module Service = struct
  type +'a io = 'a

  type error = |

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .

  type nonrec configuration = configuration =
    { inet_addr : Unix.inet_addr
    ; port : int
    ; capacity : int }

  type t = service

  type flow = protocol =
    { socket : Unix.file_descr
    ; sockaddr : Unix.sockaddr
    ; linger : Bytes.t
    ; mutable closed : bool }

  (* TODO(dinosaure): handle exception. *)

  let init { inet_addr; port; capacity; } =
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.bind socket (Unix.ADDR_INET (inet_addr, port)) ;
    Unix.listen socket capacity ; Ok socket

  let accept master =
    let socket, sockaddr = Unix.accept master in
    let linger = Bytes.create 0x1000 in
    Ok { socket; sockaddr; linger; closed= false; }

  let close master =
    Unix.close master ; Ok ()
end

let service = Conduit_unix.Service.register ~service:(module Service)
