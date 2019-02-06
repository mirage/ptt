open Cmdliner

let commands = [ ]

let run = `Help (`Pager, None)

let command =
  let doc = "TTF Solystic tool, Maildir operation." in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "TTF Solystic: Trieise de Tournées de Facteurs" ] in
  Term.(ret (const run)), Term.info "ttf-solystic" ~doc ~exits ~man

let () =
  Term.(exit @@ eval_choice command commands)
