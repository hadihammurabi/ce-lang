let dump code functions =
  Opcode.dump code;
  if List.length functions > 0 then begin
    List.iter (fun (name, body) ->
      Printf.printf "\n[%s]\n" name;
      List.iter (fun expr ->
        let compiled = Compiler.compile_expr_to_program expr in
        Opcode.dump compiled.code
      ) body
    ) functions
  end
