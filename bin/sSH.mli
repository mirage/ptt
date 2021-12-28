include Mirage_flow.S

type endpoint = string * string * string * [ `Rd | `Wr ]

val connect : endpoint -> (flow, write_error) result Lwt.t
