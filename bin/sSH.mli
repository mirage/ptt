include Mirage_flow.S

type endpoint = string * [ `host ] Domain_name.t * string * [ `Rd | `Wr ]

val connect : endpoint -> (flow, write_error) result Lwt.t
