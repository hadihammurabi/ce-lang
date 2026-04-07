let compilers = [ "gcc"; "clang"; "cc" ]

let find_compiler () =
  let check cmd =
    let null_out = if Sys.win32 then "NUL" else "/dev/null" in
    Sys.command (Printf.sprintf "%s --version > %s 2>&1" cmd null_out) = 0
  in
  try List.find check compilers
  with Not_found ->
    Printf.eprintf "Error: No C compiler found (checked: %s)\n"
      (String.concat ", " compilers);
    exit 1

let compile_and_link output_file c_file =
  let cc = find_compiler () in
  let compile_cmd =
    Printf.sprintf "%s -c %s -o %s.o -fPIC" cc c_file output_file
  in
  let link_cmd = Printf.sprintf "%s -o %s %s.o" cc output_file output_file in
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
