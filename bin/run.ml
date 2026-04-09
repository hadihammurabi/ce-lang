open Ce_compiler
open Cmdliner

let execute file =
  let binary_name = Build.remove_extension file in
  let visited = Hashtbl.create 10 in
  let ast = Build.process_file visited file in
  let _ =
    try ast |> Compiler.compile |> Compiler.export binary_name
    with Failure msg ->
      Printf.printf "Error: %s\n" msg;
      exit 1
  in

  "./" ^ binary_name |> Sys.command |> fun code -> if code <> 0 then exit code

let command =
  let doc = "Compile inserted ce-lang code file then execute that" in
  let info = Cmd.info "run" ~doc in
  Cmd.v info Term.(const execute $ Command.file_arg)
