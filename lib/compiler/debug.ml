open Ce_parser.Ast
open Opcode

let dump (code : opcode array) (functions : (string * types * stmt list) list) =
  Printf.printf "=== Global Bytecode ===\n";
  Opcode.dump code;

  if List.length functions > 0 then begin
    Printf.printf "\n=== Registered Functions ===\n";
    List.iter
      (fun (name, ty, _body) ->
        let type_str =
          match ty with
          | TypeInt -> "int"
          | TypeFloat -> "float"
          | TypeVoid -> "void"
          | TypeBool -> "bool"
          | TypeArray (length, ty) ->
              Printf.sprintf "[%d]%s" length (show_types ty)
        in
        Printf.printf "Function: %s -> %s\n" name type_str;
        Printf.printf "  [AST Statement Count: %d]\n" (List.length _body))
      functions
  end
  else Printf.printf "\nNo user-defined functions found.\n"
