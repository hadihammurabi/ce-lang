let rec compile_expr_to_c oc = function
  | Ce_parser.Ast.Int n ->
    Printf.fprintf oc "    push_int(%LdL);\n" (Int64.of_int n)
  | Ce_parser.Ast.Float f ->
    Printf.fprintf oc "    push_float(%.17g);\n" f
  | Ce_parser.Ast.String s ->
    Printf.fprintf oc "    push_string(\"%s\");\n" (String.escaped s)
  | Ce_parser.Ast.Add (l, r) ->
    compile_expr_to_c oc l;
    compile_expr_to_c oc r;
    output_string oc "    { Value r = pop(); Value l = pop();\n";
    output_string oc "      if(l.type == 0 && r.type == 0) push_int(l.value.i + r.value.i);\n";
    output_string oc "      else { double lf = (l.type == 0) ? l.value.i : l.value.f;\n";
    output_string oc "             double rf = (r.type == 0) ? r.value.i : r.value.f;\n";
    output_string oc "             push_float(lf + rf); } }\n"
  | Ce_parser.Ast.Sub (l, r) ->
    compile_expr_to_c oc l;
    compile_expr_to_c oc r;
    output_string oc "    { Value r = pop(); Value l = pop();\n";
    output_string oc "      if(l.type == 0 && r.type == 0) push_int(l.value.i - r.value.i);\n";
    output_string oc "      else { double lf = (l.type == 0) ? l.value.i : l.value.f;\n";
    output_string oc "             double rf = (r.type == 0) ? r.value.i : r.value.f;\n";
    output_string oc "             push_float(lf - rf); } }\n"
  | Ce_parser.Ast.Mul (l, r) ->
    compile_expr_to_c oc l;
    compile_expr_to_c oc r;
    output_string oc "    { Value r = pop(); Value l = pop();\n";
    output_string oc "      if(l.type == 0 && r.type == 0) push_int(l.value.i * r.value.i);\n";
    output_string oc "      else { double lf = (l.type == 0) ? l.value.i : l.value.f;\n";
    output_string oc "             double rf = (r.type == 0) ? r.value.i : r.value.f;\n";
    output_string oc "             push_float(lf * rf); } }\n"
  | Ce_parser.Ast.Div (l, r) ->
    compile_expr_to_c oc l;
    compile_expr_to_c oc r;
    output_string oc "    { Value r = pop(); Value l = pop();\n";
    output_string oc "      if(l.type == 0 && r.type == 0) push_int(l.value.i / r.value.i);\n";
    output_string oc "      else { double lf = (l.type == 0) ? l.value.i : l.value.f;\n";
    output_string oc "             double rf = (r.type == 0) ? r.value.i : r.value.f;\n";
    output_string oc "             push_float(lf / rf); } }\n"
  | Ce_parser.Ast.Neg e ->
    compile_expr_to_c oc e;
    output_string oc "    { Value v = pop();\n";
    output_string oc "      if(v.type == 0) push_int(-v.value.i);\n";
    output_string oc "      else push_float(-v.value.f); }\n"
  | Ce_parser.Ast.Call (name, args) ->
    List.iter (compile_expr_to_c oc) args;
    let argc = List.length args in
    (match name with
    | "println" -> Printf.fprintf oc "    builtin_println(%d);\n" argc
    | "print" -> Printf.fprintf oc "    builtin_print(%d);\n" argc
    | _ -> Printf.fprintf oc "    %s();\n" name)
  | Ce_parser.Ast.Var (name) ->
    Printf.fprintf oc "    stack[sp] = %s;\n" name;
    Printf.fprintf oc "    sp++;\n"

let write_c_wrapper filename code functions globals =
  let oc = open_out filename in
  output_string oc "#include <stdio.h>\n";
  output_string oc "#include <stdint.h>\n";
  output_string oc "#include <string.h>\n";
  output_string oc "#include <stdlib.h>\n\n";
  
  output_string oc "#define STACK_SIZE 10000\n\n";
  
  output_string oc "typedef struct {\n";
  output_string oc "    int type;\n";
  output_string oc "    union {\n";
  output_string oc "        int64_t i;\n";
  output_string oc "        double f;\n";
  output_string oc "        char *s;\n";
  output_string oc "    } value;\n";
  output_string oc "} Value;\n\n";
  
  output_string oc "static Value stack[STACK_SIZE];\n";
  output_string oc "static int sp = 0;\n\n";
  
  output_string oc "void push_int(int64_t n) {\n";
  output_string oc "    stack[sp].type = 0;\n";
  output_string oc "    stack[sp].value.i = n;\n";
  output_string oc "    sp++;\n";
  output_string oc "}\n\n";
  
  output_string oc "void push_float(double f) {\n";
  output_string oc "    stack[sp].type = 1;\n";
  output_string oc "    stack[sp].value.f = f;\n";
  output_string oc "    sp++;\n";
  output_string oc "}\n\n";
  
  output_string oc "void push_string(const char *s) {\n";
  output_string oc "    stack[sp].type = 2;\n";
  output_string oc "    stack[sp].value.s = (char*)malloc(strlen(s) + 1);\n";
  output_string oc "    strcpy(stack[sp].value.s, s);\n";
  output_string oc "    sp++;\n";
  output_string oc "}\n\n";
  
  output_string oc "Value pop() {\n";
  output_string oc "    return stack[--sp];\n";
  output_string oc "}\n\n";

  output_string oc "void builtin_println(int argc) {\n";
  output_string oc "    for(int i = argc; i > 0; i--) {\n";
  output_string oc "        Value v = stack[sp - i];\n";
  output_string oc "        switch(v.type) {\n";
  output_string oc "            case 0: printf(\"%ld\", v.value.i); break;\n";
  output_string oc "            case 1: printf(\"%g\", v.value.f); break;\n";
  output_string oc "            case 2: printf(\"%s\", v.value.s); break;\n";
  output_string oc "            case 3: printf(\"()\"); break;\n";
  output_string oc "        }\n";
  output_string oc "        if (i > 1) printf(\" \");\n";
  output_string oc "    }\n";
  output_string oc "    printf(\"\\n\");\n";
  output_string oc "    sp -= argc;\n";
  output_string oc "}\n\n";
  
  output_string oc "void builtin_print(int argc) {\n";
  output_string oc "    for(int i = argc; i > 0; i--) {\n";
  output_string oc "        Value v = stack[sp - i];\n";
  output_string oc "        switch(v.type) {\n";
  output_string oc "            case 0: printf(\"%ld\", v.value.i); break;\n";
  output_string oc "            case 1: printf(\"%g\", v.value.f); break;\n";
  output_string oc "            case 2: printf(\"%s\", v.value.s); break;\n";
  output_string oc "            case 3: printf(\"()\"); break;\n";
  output_string oc "        }\n";
  output_string oc "        if (i > 1) printf(\" \");\n";
  output_string oc "    }\n";
  output_string oc "    sp -= argc;\n";
  output_string oc "}\n\n";

  List.iter (fun (fname, _body) ->
    let fn_name = if fname = "main" then "fn_main" else fname in
    Printf.fprintf oc "void %s();\n" fn_name
  ) functions;
  output_string oc "\n";

  List.iter (fun (name, _ty, _expr) ->
    Printf.fprintf oc "Value %s;\n" name
  ) globals;
  output_string oc "\n";
  
  List.iter (fun (fname, body) ->
    let fn_name = if fname = "main" then "fn_main" else fname in
    Printf.fprintf oc "void %s() {\n" fn_name;
    Printf.fprintf oc "    sp = 0;\n\n";
    List.iter (fun expr ->
      compile_expr_to_c oc expr
    ) body;
    output_string oc "}\n\n"
  ) functions;
  
  output_string oc "int main() {\n";

  List.iter (fun (name, _ty, expr) ->
    Printf.fprintf oc "    sp = 0;\n";
    compile_expr_to_c oc expr;
    Printf.fprintf oc "    %s = pop();\n\n" name
  ) globals;

  output_string oc "    fn_main();\n";
  output_string oc "    return 0;\n";
  output_string oc "}\n";
  
  close_out oc
