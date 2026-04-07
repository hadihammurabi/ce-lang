open Ce_parser

let rec compile_expr_to_c oc = function
  | Ast.Int n -> Printf.fprintf oc "    push_int(%LdL);\n" (Int64.of_int n)
  | Ast.Float f -> Printf.fprintf oc "    push_float(%.17g);\n" f
  | Ast.String s ->
      Printf.fprintf oc "    push_string(\"%s\");\n" (String.escaped s)
  | Ast.Bool b -> Printf.fprintf oc "    push_bool(%d);\n" (if b then 1 else 0)
  | Ast.Add (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      output_string oc "    { Value r = pop(); Value l = pop();\n";
      output_string oc
        "      if(l.type == 0 && r.type == 0) push_int(l.value.i + r.value.i);\n";
      output_string oc
        "      else { double lf = (l.type == 0) ? l.value.i : l.value.f;\n";
      output_string oc
        "             double rf = (r.type == 0) ? r.value.i : r.value.f;\n";
      output_string oc "             push_float(lf + rf); } }\n"
  | Ast.Sub (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      output_string oc "    { Value r = pop(); Value l = pop();\n";
      output_string oc
        "      if(l.type == 0 && r.type == 0) push_int(l.value.i - r.value.i);\n";
      output_string oc
        "      else { double lf = (l.type == 0) ? l.value.i : l.value.f;\n";
      output_string oc
        "             double rf = (r.type == 0) ? r.value.i : r.value.f;\n";
      output_string oc "             push_float(lf - rf); } }\n"
  | Ast.Mul (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      output_string oc "    { Value r = pop(); Value l = pop();\n";
      output_string oc
        "      if(l.type == 0 && r.type == 0) push_int(l.value.i * r.value.i);\n";
      output_string oc
        "      else { double lf = (l.type == 0) ? l.value.i : l.value.f;\n";
      output_string oc
        "             double rf = (r.type == 0) ? r.value.i : r.value.f;\n";
      output_string oc "             push_float(lf * rf); } }\n"
  | Ast.Div (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      output_string oc "    { Value r = pop(); Value l = pop();\n";
      output_string oc
        "      if(l.type == 0 && r.type == 0) push_int(l.value.i / r.value.i);\n";
      output_string oc
        "      else { double lf = (l.type == 0) ? l.value.i : l.value.f;\n";
      output_string oc
        "             double rf = (r.type == 0) ? r.value.i : r.value.f;\n";
      output_string oc "             push_float(lf / rf); } }\n"
  | Ast.Eq (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      output_string oc "    { Value r = pop(); Value l = pop();\n";
      output_string oc
        "      if(l.type == 0 && r.type == 0) push_bool(l.value.i == r.value.i);\n";
      output_string oc
        "      else { double lf = (l.type == 0) ? l.value.i : l.value.f;\n";
      output_string oc
        "             double rf = (r.type == 0) ? r.value.i : r.value.f;\n";
      output_string oc "             push_bool(lf == rf); } }\n"
  | Ast.Lt (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      output_string oc "    { Value r = pop(); Value l = pop();\n";
      output_string oc
        "      if(l.type == 0 && r.type == 0) push_bool(l.value.i < r.value.i);\n";
      output_string oc
        "      else { double lf = (l.type == 0) ? l.value.i : l.value.f;\n";
      output_string oc
        "             double rf = (r.type == 0) ? r.value.i : r.value.f;\n";
      output_string oc "             push_bool(lf < rf); } }\n"
  | Ast.Gt (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      output_string oc "    { Value r = pop(); Value l = pop();\n";
      output_string oc
        "      if(l.type == 0 && r.type == 0) push_bool(l.value.i > r.value.i);\n";
      output_string oc
        "      else { double lf = (l.type == 0) ? l.value.i : l.value.f;\n";
      output_string oc
        "             double rf = (r.type == 0) ? r.value.i : r.value.f;\n";
      output_string oc "             push_bool(lf > rf); } }\n"
  | Ast.Lte (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      output_string oc "    { Value r = pop(); Value l = pop();\n";
      output_string oc
        "      if(l.type == 0 && r.type == 0) push_bool(l.value.i <= r.value.i);\n";
      output_string oc
        "      else { double lf = (l.type == 0) ? l.value.i : l.value.f;\n";
      output_string oc
        "             double rf = (r.type == 0) ? r.value.i : r.value.f;\n";
      output_string oc "             push_bool(lf <= rf); } }\n"
  | Ast.Gte (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      output_string oc "    { Value r = pop(); Value l = pop();\n";
      output_string oc
        "      if(l.type == 0 && r.type == 0) push_bool(l.value.i >= r.value.i);\n";
      output_string oc
        "      else { double lf = (l.type == 0) ? l.value.i : l.value.f;\n";
      output_string oc
        "             double rf = (r.type == 0) ? r.value.i : r.value.f;\n";
      output_string oc "             push_bool(lf >= rf); } }\n"
  | Ast.And (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      output_string oc "    { Value r = pop(); Value l = pop();\n";
      output_string oc
        "      int lv = (l.type == 0) ? (l.value.i != 0) : (l.value.f != 0.0);\n";
      output_string oc
        "      int rv = (r.type == 0) ? (r.value.i != 0) : (r.value.f != 0.0);\n";
      output_string oc "      push_bool(lv && rv); }\n"
  | Ast.Or (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      output_string oc "    { Value r = pop(); Value l = pop();\n";
      output_string oc
        "      int lv = (l.type == 0) ? (l.value.i != 0) : (l.value.f != 0.0);\n";
      output_string oc
        "      int rv = (r.type == 0) ? (r.value.i != 0) : (r.value.f != 0.0);\n";
      output_string oc "      push_bool(lv || rv); }\n"
  | Ast.Neg e ->
      compile_expr_to_c oc e;
      output_string oc "    { Value v = pop();\n";
      output_string oc "      if(v.type == 0) push_int(-v.value.i);\n";
      output_string oc "      else push_float(-v.value.f); }\n"
  | Ast.Call (name, args) -> (
      List.iter (compile_expr_to_c oc) args;
      let argc = List.length args in
      match name with
      | "println" -> Printf.fprintf oc "    builtin_println(%d);\n" argc
      | "print" -> Printf.fprintf oc "    builtin_print(%d);\n" argc
      | _ ->
          output_string oc "    {\n";
          let arg_names =
            List.mapi (fun i _ -> Printf.sprintf "__arg%d" i) args
          in
          List.iter
            (fun aname -> Printf.fprintf oc "        Value %s = pop();\n" aname)
            (List.rev arg_names);
          let call_args = String.concat ", " arg_names in
          Printf.fprintf oc "        %s(%s);\n" name call_args;
          output_string oc "    }\n")
  | Ast.Var name ->
      Printf.fprintf oc "    stack[sp] = %s;\n" name;
      Printf.fprintf oc "    sp++;\n"
  | Ast.Array (n, ty, elems) ->
      List.iter (compile_expr_to_c oc) elems;
      List.iter (compile_expr_to_c oc) elems;
      Printf.fprintf oc "    {\n";
      Printf.fprintf oc "        int __arr_size = %d;\n" n;
      Printf.fprintf oc
        "        Value *__arr = (Value*)malloc(__arr_size * sizeof(Value));\n";
      Printf.fprintf oc
        "        for(int __i = __arr_size - 1; __i >= 0; __i--)\n";
      Printf.fprintf oc "            __arr[__i] = pop();\n";
      Printf.fprintf oc "        stack[sp].type           = 3;\n";
      Printf.fprintf oc "        stack[sp].value.arr.data = __arr;\n";
      Printf.fprintf oc "        stack[sp].value.arr.len  = __arr_size;\n";
      Printf.fprintf oc "        sp++;\n";
      Printf.fprintf oc "    }\n"
  | Ast.If (cond, then_body, elif_branches, else_body) ->
      output_string oc "    {\n";
      compile_expr_to_c oc cond;
      output_string oc "    Value __cond = pop();\n";
      output_string oc
        "    int __cv = (__cond.type == 0) ? (__cond.value.i != 0)\n";
      output_string oc
        "             : (__cond.type == 4) ? (__cond.value.i != 0)\n";
      output_string oc "             : (__cond.value.f != 0.0);\n";
      output_string oc "    if (__cv) {\n";
      List.iter (compile_stmt_to_c oc) then_body;
      output_string oc "    }\n";
      List.iter
        (fun (econd, ebody) ->
          output_string oc "    else {\n";
          compile_expr_to_c oc econd;
          output_string oc "    Value __cond = pop();\n";
          output_string oc
            "    int __cv = (__cond.type == 0) ? (__cond.value.i != 0)\n";
          output_string oc
            "             : (__cond.type == 4) ? (__cond.value.i != 0)\n";
          output_string oc "             : (__cond.value.f != 0.0);\n";
          output_string oc "    if (__cv) {\n";
          List.iter (compile_stmt_to_c oc) ebody;
          output_string oc "    }\n")
        elif_branches;
      (match else_body with
      | Some stmts ->
          output_string oc "    else {\n";
          List.iter (compile_stmt_to_c oc) stmts;
          output_string oc "    }\n"
      | None -> ());
      List.iter (fun _ -> output_string oc "    }\n") elif_branches;
      output_string oc "    }\n"

and compile_stmt_to_c oc = function
  | Ast.Expr e -> compile_expr_to_c oc e
  | Ast.DefVar (name, _ty, expr) ->
      compile_expr_to_c oc expr;
      Printf.fprintf oc "    Value %s = pop();\n" name
  | Ast.DefFN _ -> ()
  | Ast.Return e ->
      compile_expr_to_c oc e;
      Printf.fprintf oc "    return;\n"
  | Ast.Block body ->
      output_string oc "    {\n";
      List.iter (compile_stmt_to_c oc) body;
      output_string oc "    }\n"

let write_c_wrapper filename code functions globals =
  let oc = open_out filename in
  output_string oc "#include <stdio.h>\n";
  output_string oc "#include <stdint.h>\n";
  output_string oc "#include <string.h>\n";
  output_string oc "#include <stdlib.h>\n\n";

  output_string oc "#define STACK_SIZE 10000\n\n";
  output_string oc "typedef struct Value Value;\n";
  output_string oc "struct Value {\n";
  output_string oc "    int type;\n";
  output_string oc "    union {\n";
  output_string oc "        int64_t i;\n";
  output_string oc "        double  f;\n";
  output_string oc "        char   *s;\n";
  output_string oc "        struct { Value *data; int len; } arr;\n";
  output_string oc "    } value;\n";
  output_string oc "};\n\n";
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
  output_string oc "            case 3: {\n";
  output_string oc "                printf(\"[\");\n";
  output_string oc
    "                for(int j = 0; j < v.value.arr.len; j++) {\n";
  output_string oc "                    Value e = v.value.arr.data[j];\n";
  output_string oc
    "                    if(e.type==0) printf(\"%ld\", e.value.i);\n";
  output_string oc
    "                    else if(e.type==1) printf(\"%g\", e.value.f);\n";
  output_string oc
    "                    else if(e.type==2) printf(\"%s\", e.value.s);\n";
  output_string oc
    "                    if(j < v.value.arr.len - 1) printf(\", \");\n";
  output_string oc "                }\n";
  output_string oc "                printf(\"]\");\n";
  output_string oc "                break; }\n";
  output_string oc
    "            case 4: printf(\"%s\", v.value.i ? \"true\" : \"false\"); \
     break;\n";
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
  output_string oc "            case 3: {\n";
  output_string oc "                printf(\"[\");\n";
  output_string oc
    "                for(int j = 0; j < v.value.arr.len; j++) {\n";
  output_string oc "                    Value e = v.value.arr.data[j];\n";
  output_string oc
    "                    if(e.type==0) printf(\"%ld\", e.value.i);\n";
  output_string oc
    "                    else if(e.type==1) printf(\"%g\", e.value.f);\n";
  output_string oc
    "                    else if(e.type==2) printf(\"%s\", e.value.s);\n";
  output_string oc
    "                    if(j < v.value.arr.len - 1) printf(\", \");\n";
  output_string oc "                }\n";
  output_string oc
    "            case 4: printf(\"%s\", v.value.i ? \"true\" : \"false\"); \
     break;\n";
  output_string oc "                printf(\"]\");\n";
  output_string oc "                break; }\n";
  output_string oc "        }\n";
  output_string oc "        if (i > 1) printf(\" \");\n";
  output_string oc "    }\n";
  output_string oc "    sp -= argc;\n";
  output_string oc "}\n\n";

  output_string oc "void push_bool(int b) {\n";
  output_string oc "    stack[sp].type = 4;\n";
  output_string oc "    stack[sp].value.i = b;\n";
  output_string oc "    sp++;\n";
  output_string oc "}\n\n";

  List.iter
    (fun (name, _ty, _expr) -> Printf.fprintf oc "Value %s;\n" name)
    globals;
  output_string oc "\n";

  List.iter
    (fun (fname, params, _ty, _body) ->
      let fn_name = if fname = "main" then "fn_main" else fname in
      let param_str =
        if params = [] then "void"
        else
          String.concat ", "
            (List.map (fun p -> Printf.sprintf "Value %s" p.Ast.name) params)
      in
      Printf.fprintf oc "void %s(%s);\n" fn_name param_str)
    functions;
  output_string oc "\n";

  List.iter
    (fun (fname, params, _ty, body) ->
      let fn_name = if fname = "main" then "fn_main" else fname in
      let param_str =
        if params = [] then "void"
        else
          String.concat ", "
            (List.map (fun p -> Printf.sprintf "Value %s" p.Ast.name) params)
      in
      Printf.fprintf oc "void %s(%s) {\n" fn_name param_str;
      List.iter (compile_stmt_to_c oc) body;
      output_string oc "}\n\n")
    functions;

  output_string oc "int main() {\n";

  List.iter
    (fun (name, _ty, expr) ->
      Printf.fprintf oc "    sp = 0;\n";
      compile_expr_to_c oc expr;
      Printf.fprintf oc "    %s = pop();\n\n" name)
    globals;

  output_string oc "    fn_main();\n";
  output_string oc "    return 0;\n";
  output_string oc "}\n";

  close_out oc
