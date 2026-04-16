open Llvm_target
open Ce_parser.Ast
open Llvm

let dump m =
  let module_string = string_of_llmodule m in
  print_endline module_string

let export binary_name the_module =
  ignore (Llvm_all_backends.initialize ());
  let target_triple = Target.default_triple () in
  let target = Target.by_triple target_triple in
  let machine =
    TargetMachine.create ~triple:target_triple ~reloc_mode:RelocMode.PIC target
  in

  let obj_filename = binary_name ^ ".o" in
  TargetMachine.emit_to_file the_module CodeGenFileType.ObjectFile obj_filename
    machine;

  let link_cmd = Printf.sprintf "cc %s -lgc -o %s" obj_filename binary_name in
  match Sys.command link_cmd with
  | 0 ->
      if Sys.file_exists obj_filename then Sys.remove obj_filename;
      ()
  | code ->
      Printf.eprintf "Linking failed with code %d\n" code;
      exit 1
