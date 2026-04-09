open Ce_compiler
open Cmdliner

let execute file =
  let visited = Hashtbl.create 10 in
  let ast = Build.process_file visited file in
  try
    let prog = ast |> Compiler.compile in
    Compiler.dump prog
  with Failure msg ->
    Printf.printf "Error: %s\n" msg;
    exit 1

let command =
  let doc = "Read ce-lang code file then show debug output" in
  let info = Cmd.info "debug" ~doc in
  Cmd.v info Term.(const execute $ Command.file_arg)
