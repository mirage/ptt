let src = Logs.Src.create "ptt.server"

module Log = (val Logs.src_log src)

module Make (Stack : Tcpip.Stack.V4V6) = struct
  open Lwt.Infix

  type service = {
      stack: Stack.TCP.t
    ; queue: Stack.TCP.flow Queue.t
    ; condition: unit Lwt_condition.t
    ; mutex: Lwt_mutex.t
    ; mutable closed: bool
  }

  let init ~port stack =
    let queue = Queue.create () in
    let condition = Lwt_condition.create () in
    let mutex = Lwt_mutex.create () in

    let listener flow =
      let ipaddr, port = Stack.TCP.dst flow in
      Lwt_mutex.with_lock mutex @@ fun () ->
      Log.debug (fun m ->
          m "A new incoming connection: %a:%d" Ipaddr.pp ipaddr port);
      Queue.push flow queue;
      Lwt_condition.signal condition ();
      Lwt.return_unit in
    Stack.TCP.listen ~port stack listener;
    Lwt.return {stack; queue; condition; mutex; closed= false}

  let rec accept ({queue; condition; mutex; _} as t) =
    let rec await () =
      if Queue.is_empty queue && not t.closed then
        Lwt_condition.wait condition ~mutex >>= Lwt.pause >>= await
      else Lwt.return (Queue.take_opt queue, t.closed) in
    Lwt_mutex.with_lock mutex await >>= function
    | Some flow, _ -> Lwt.return (`Flow flow)
    | None, false -> accept t
    | None, true -> Lwt.return `Closed

  let close ({stack; condition; mutex; _} as t) =
    Lwt_mutex.with_lock mutex @@ fun () ->
    Log.debug (fun m -> m "Close the server");
    t.closed <- true;
    Stack.TCP.disconnect stack >>= fun () ->
    Lwt_condition.broadcast condition ();
    Lwt.return_unit

  let rec clean acc = function
    | [] -> acc
    | th :: ths -> (
      match Lwt.state th with
      | Return () -> clean acc ths
      | Fail exn ->
        Log.err (fun m ->
            m "A spawned thread failed with: %S" (Printexc.to_string exn));
        clean acc ths
      | Sleep -> clean (th :: acc) ths)

  let rec terminate = function
    | [] ->
      Log.debug (fun m -> m "The server is cleaned");
      Lwt.return_unit
    | th :: ths -> (
      Log.debug (fun m -> m "Unterminated tasks");
      match Lwt.state th with
      | Return () -> terminate ths
      | Fail exn ->
        Log.err (fun m ->
            m "A spawned thread failed with: %S" (Printexc.to_string exn));
        terminate ths
      | Sleep -> Lwt.pause () >>= fun () -> terminate (th :: ths))

  let serve_when_ready ?stop ~handler service =
    `Initialized
      (Lwt_switch.add_hook stop (fun () -> close service);
       let rec loop ths =
         let ths = clean [] ths in
         accept service >>= function
         | `Flow flow ->
           let th = handler flow in
           loop (th :: ths)
         | `Closed ->
           Log.debug (fun m -> m "Terminate the server");
           Lwt.return (clean [] ths) in
       loop [] >>= terminate)
end
