open Ce_parser.Ast
open Ce_compiler.Opcode

let dump (code : opcode array)
    (functions : (string * param list * types * stmt list) list) =
  Printf.printf "=== Global Bytecode ===\n";
  dump code;

  if List.length functions > 0 then begin
    Printf.printf "\n=== Registered Functions ===\n";
    List.iter
      (fun (name, params, ty, _body) ->
        let type_str =
          match ty with
          | TInt -> "int"
          | TFloat -> "float"
          | TVoid -> "void"
          | TString -> "string"
          | TBool -> "bool"
          | TUnknown -> "unknown"
          | TArray (length, ty) ->
              Printf.sprintf "[%d]%s" length (show_types ty)
        in
        Printf.printf "Function: %s -> %s\n" name type_str;
        Printf.printf "  [AST Statement Count: %d]\n" (List.length _body))
      functions
  end
  else Printf.printf "\nNo user-defined functions found.\n"
