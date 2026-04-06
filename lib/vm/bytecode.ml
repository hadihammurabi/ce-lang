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
    (match name with
    | "println" -> output_string oc "    builtin_println();\n"
    | "print" -> output_string oc "    builtin_print();\n"
    | _ -> Printf.fprintf oc "    /* unknown function: %s */\n" name)

let write_c_wrapper filename prog functions =
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
  
  output_string oc "void builtin_println() {\n";
  output_string oc "    Value v = pop();\n";
  output_string oc "    switch(v.type) {\n";
  output_string oc "        case 0: printf(\"%ld\\n\", v.value.i); break;\n";
  output_string oc "        case 1: printf(\"%g\\n\", v.value.f); break;\n";
  output_string oc "        case 2: printf(\"%s\\n\", v.value.s); break;\n";
  output_string oc "        case 3: printf(\"()\\n\"); break;\n";
  output_string oc "    }\n";
  output_string oc "}\n\n";
  
  output_string oc "void builtin_print() {\n";
  output_string oc "    Value v = pop();\n";
  output_string oc "    switch(v.type) {\n";
  output_string oc "        case 0: printf(\"%ld\", v.value.i); break;\n";
  output_string oc "        case 1: printf(\"%g\", v.value.f); break;\n";
  output_string oc "        case 2: printf(\"%s\", v.value.s); break;\n";
  output_string oc "        case 3: printf(\"()\"); break;\n";
  output_string oc "    }\n";
  output_string oc "}\n\n";
  
  List.iter (fun (fname, body) ->
    Printf.fprintf oc "void fn_%s() {\n" fname;
    Printf.fprintf oc "    sp = 0;\n\n";
    List.iter (fun expr ->
      compile_expr_to_c oc expr
    ) body;
    output_string oc "}\n\n"
  ) functions;
  
  output_string oc "int main() {\n";
  output_string oc "    fn_main();\n";
  output_string oc "    return 0;\n";
  output_string oc "}\n";
  
  close_out oc
