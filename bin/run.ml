open Cmdliner

let execute filename =
   Printf.printf "Running file: %s\n" filename;
   Printf.printf "cp %s %s\n" filename "mantap"

let command =
  let doc = "Compile inserted ce-lang code file then execute that" in
  let info = Cmd.info "run" ~doc in
  Cmd.v info Term.(const execute $ Command.file_arg)
