let compile_and_link output_file c_file =
  let compile_cmd =
    Printf.sprintf "gcc -c %s -o %s.o -fPIC" c_file output_file
  in
  let link_cmd = Printf.sprintf "gcc -o %s %s.o" output_file output_file in
  match Sys.command compile_cmd with
  | 0 -> (
      match Sys.command link_cmd with
      | 0 -> ()
      | code ->
          Printf.eprintf "Link failed with code %d\n" code;
          exit 1)
  | code ->
      Printf.eprintf "Compilation failed with code %d\n" code;
      exit 1

let cleanup_temp base_name =
  let obj_file = base_name ^ ".o" in
  let c_file = base_name ^ ".c" in
  if Sys.file_exists obj_file then Sys.remove obj_file;
  if Sys.file_exists c_file then Sys.remove c_file
