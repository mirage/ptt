include Mirage_flow.S

type endpoint = {
    user: string
  ; path: string
  ; host: Unix.inet_addr
  ; port: int
  ; capabilities: [ `Rd | `Wr ]
}

val connect : endpoint -> (flow, write_error) result Lwt.t
