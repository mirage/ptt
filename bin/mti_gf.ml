let () = Printexc.record_backtrace true

open Cmdliner

let commands = [ Verify.command ]

let run = `Help (`Pager, None)

let command =
  let doc = "MTI-GF tool." in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "MTI-GFI: Machine de Tri Industriel" ] in
  Term.(ret (const run)), Term.info "mti-gf" ~doc ~exits ~man

let () =
  Term.(exit @@ eval_choice command commands)
