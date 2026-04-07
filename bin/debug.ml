open Ce_compiler
open Cmdliner

let execute file bytecode =
  let prog = file |> Build.read |> Build.compile in
  let base_name =
    try String.sub file 0 (String.rindex file '.') with Not_found -> file in
  let c_file = base_name ^ ".c" in
  Bytecode.write_c_wrapper c_file prog.code prog.functions prog.globals;

  c_file |> Build.read |> print_endline;

  Linker.cleanup_temp c_file

let bytecode_arg =
  let doc = "Print the generated C code to stdout" in
  Arg.(value & flag & info [ "bytecode"; "bc" ] ~doc)

let command =
  let doc = "Read ce-lang code file then show debug output" in
  let info = Cmd.info "debug" ~doc in
  Cmd.v info Term.(const execute $ Command.file_arg $ bytecode_arg)
