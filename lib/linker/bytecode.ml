open Ce_parser

let c_safe name = String.map (fun c -> if c = '.' then '_' else c) name

let c_type_of = function
  | Ast.TypeInt -> "int64_t"
  | Ast.TypeFloat -> "double"
  | Ast.TypeBool -> "int64_t"
  | Ast.TypeString -> "char*"
  | Ast.TypeVoid -> "void"
  | _ -> "Value"

let rec compile_expr_to_c oc (e : Ast.expr) =
  match e.kind with
  | Ast.Void -> ()
  | Ast.Int n -> Printf.fprintf oc "    push_int(%LdL);\n" (Int64.of_int n)
  | Ast.Float f -> Printf.fprintf oc "    push_float(%.17g);\n" f
  | Ast.String s ->
      Printf.fprintf oc "    push_string(\"%s\");\n" (String.escaped s)
  | Ast.Bool b -> Printf.fprintf oc "    push_bool(%d);\n" (if b then 1 else 0)
  | Ast.Add (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      if e.ty = Ast.TypeInt then
        output_string oc
          "    { int64_t r = pop().value.i; int64_t l = pop().value.i; \
           push_int(l + r); }\n"
      else
        output_string oc
          "    { double r = pop().value.f; double l = pop().value.f; \
           push_float(l + r); }\n"
  | Ast.Sub (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      if e.ty = Ast.TypeInt then
        output_string oc
          "    { int64_t r = pop().value.i; int64_t l = pop().value.i; \
           push_int(l - r); }\n"
      else
        output_string oc
          "    { double r = pop().value.f; double l = pop().value.f; \
           push_float(l - r); }\n"
  | Ast.Mul (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      if e.ty = Ast.TypeInt then
        output_string oc
          "    { int64_t r = pop().value.i; int64_t l = pop().value.i; \
           push_int(l * r); }\n"
      else
        output_string oc
          "    { double r = pop().value.f; double l = pop().value.f; \
           push_float(l * r); }\n"
  | Ast.Div (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      if e.ty = Ast.TypeInt then
        output_string oc
          "    { int64_t r = pop().value.i; int64_t l = pop().value.i; \
           push_int(l / r); }\n"
      else
        output_string oc
          "    { double r = pop().value.f; double l = pop().value.f; \
           push_float(l / r); }\n"
  | Ast.Mod (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      if e.ty = Ast.TypeInt then
        output_string oc
          "    { int64_t r = pop().value.i; int64_t l = pop().value.i; \
           push_int(l % r); }\n"
      else
        output_string oc
          "    { double r = pop().value.f; double l = pop().value.f; \
           push_float(l % r); }\n"
  | Ast.Eq (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      if l.ty = Ast.TypeInt || l.ty = Ast.TypeBool then
        output_string oc
          "    { int64_t r = pop().value.i; int64_t l = pop().value.i; \
           push_bool(l == r); }\n"
      else if l.ty = Ast.TypeFloat then
        output_string oc
          "    { double r = pop().value.f; double l = pop().value.f; \
           push_bool(l == r); }\n"
      else if l.ty = Ast.TypeString then
        output_string oc
          "    { char* r = pop().value.s; char* l = pop().value.s; \
           push_bool(strcmp(l, r) == 0); }\n"
  | Ast.Lt (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      if l.ty = Ast.TypeInt then
        output_string oc
          "    { int64_t r = pop().value.i; int64_t l = pop().value.i; \
           push_bool(l < r); }\n"
      else
        output_string oc
          "    { double r = pop().value.f; double l = pop().value.f; \
           push_bool(l < r); }\n"
  | Ast.Gt (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      if l.ty = Ast.TypeInt then
        output_string oc
          "    { int64_t r = pop().value.i; int64_t l = pop().value.i; \
           push_bool(l > r); }\n"
      else
        output_string oc
          "    { double r = pop().value.f; double l = pop().value.f; \
           push_bool(l > r); }\n"
  | Ast.Lte (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      if l.ty = Ast.TypeInt then
        output_string oc
          "    { int64_t r = pop().value.i; int64_t l = pop().value.i; \
           push_bool(l <= r); }\n"
      else
        output_string oc
          "    { double r = pop().value.f; double l = pop().value.f; \
           push_bool(l <= r); }\n"
  | Ast.Gte (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      if l.ty = Ast.TypeInt then
        output_string oc
          "    { int64_t r = pop().value.i; int64_t l = pop().value.i; \
           push_bool(l >= r); }\n"
      else
        output_string oc
          "    { double r = pop().value.f; double l = pop().value.f; \
           push_bool(l >= r); }\n"
  | Ast.And (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      output_string oc
        "    { int64_t r = pop().value.i; int64_t l = pop().value.i; \
         push_bool(l && r); }\n"
  | Ast.Or (l, r) ->
      compile_expr_to_c oc l;
      compile_expr_to_c oc r;
      output_string oc
        "    { int64_t r = pop().value.i; int64_t l = pop().value.i; \
         push_bool(l || r); }\n"
  | Ast.Neg e ->
      compile_expr_to_c oc e;
      if e.ty = Ast.TypeInt then
        output_string oc "    { int64_t v = pop().value.i; push_int(-v); }\n"
      else
        output_string oc "    { double v = pop().value.f; push_float(-v); }\n"
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
          Printf.fprintf oc "        ce_%s(%s);\n" (c_safe name) call_args;
          output_string oc "    }\n")
  | Ast.Let name ->
      Printf.fprintf oc "    stack[sp] = %s;\n" (c_safe name);
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

      output_string oc "    if (pop().value.i) {\n";
      List.iter (compile_stmt_to_c oc) then_body;
      output_string oc "    }\n";

      List.iter
        (fun (econd, ebody) ->
          output_string oc "    else {\n";
          compile_expr_to_c oc econd;
          output_string oc "    if (pop().value.i) {\n";
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
  | Ast.DefLet (name, ismut, ty, expr) ->
      compile_expr_to_c oc expr;
      Printf.fprintf oc "    Value %s = pop();\n" (c_safe name)
  | Ast.DefFN _ -> ()
  | Ast.Return e ->
      compile_expr_to_c oc e;
      Printf.fprintf oc "    return pop().value.i;\n"
  | Ast.Block body ->
      output_string oc "    {\n";
      List.iter (compile_stmt_to_c oc) body;
      output_string oc "    }\n"
  | Ast.For body ->
      output_string oc "    while(1) {\n";
      List.iter (compile_stmt_to_c oc) body;
      output_string oc "    }\n"
  | Ast.Break -> output_string oc "    break;\n"
  | Ast.Assign (name, expr) ->
      compile_expr_to_c oc expr;
      Printf.fprintf oc "    %s = pop();\n" (c_safe name)
  | Ast.Import _ -> ()

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
    (fun (name, ismut, _ty, _expr) ->
      Printf.fprintf oc "Value %s;\n" (c_safe name))
    globals;
  output_string oc "\n";

  List.iter
    (fun (fname, params, ty, _body) ->
      let fn_name = "ce_" ^ c_safe fname in
      let param_str =
        if params = [] then "void"
        else
          String.concat ", "
            (List.map (fun p -> Printf.sprintf "Value %s" p.Ast.name) params)
      in
      Printf.fprintf oc "%s %s(%s);\n" (c_type_of ty) fn_name param_str)
    functions;
  output_string oc "\n";

  List.iter
    (fun (fname, params, ty, body) ->
      let fn_name = "ce_" ^ c_safe fname in
      let param_str =
        if params = [] then "void"
        else
          String.concat ", "
            (List.map (fun p -> Printf.sprintf "Value %s" p.Ast.name) params)
      in
      Printf.fprintf oc "%s %s(%s) {\n" (c_type_of ty) fn_name param_str;
      List.iter (compile_stmt_to_c oc) body;
      output_string oc "}\n\n")
    functions;

  output_string oc "int main() {\n";

  List.iter
    (fun (name, _is_mut, _ty, expr) ->
      Printf.fprintf oc "    sp = 0;\n";
      compile_expr_to_c oc expr;
      Printf.fprintf oc "    *(Value*)&%s = pop();\n\n" (c_safe name))
    globals;

  output_string oc "    ce_main();\n";
  output_string oc "    return 0;\n";
  output_string oc "}\n";

  close_out oc
