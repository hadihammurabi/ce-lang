open Ce_linker
open Ce_compiler
open Cmdliner

let execute file bytecode =
  let visited = Hashtbl.create 10 in
  let ast = Build.parse_file visited file in
  let base_name = String.sub file 0 (String.rindex file '.') in
  let c_file = base_name ^ ".c" in
  try
    let prog = ast |> Typer.check_program |> Compiler.compile in
    if bytecode then
      let _ = Bytecode.write_c_wrapper c_file prog.code prog.functions prog.globals in
      let _ = c_file |> Build.read |> print_endline in
      Linker.cleanup_temp c_file
    else
      Debug.dump prog.code prog.functions;
  with Failure msg ->
    Printf.printf "Error: %s\n" msg;
    exit 1
  

let bytecode_arg =
  let doc = "Print the generated C code to stdout" in
  Arg.(value & flag & info [ "bytecode"; "bc" ] ~doc)

let command =
  let doc = "Read ce-lang code file then show debug output" in
  let info = Cmd.info "debug" ~doc in
  Cmd.v info Term.(const execute $ Command.file_arg $ bytecode_arg)
