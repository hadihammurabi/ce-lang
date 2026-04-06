open Cmdliner

let execute action =
   Printf.printf "cp %s %s\n" action "mantap"

let command =
  let doc = "Compile inserted ce-lang code file then execute that" in
  let info = Cmd.info "run" ~doc in
  Cmd.v info Term.(const execute $ const())
