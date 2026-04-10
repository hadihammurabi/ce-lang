open Llvm

exception Error of string

let get_printf context the_module =
  match lookup_function "printf" the_module with
  | Some f -> f
  | None ->
      let printf_ty =
        var_arg_function_type (i32_type context) [| pointer_type context |]
      in
      declare_function "printf" printf_ty the_module

let get name =
  match name with
  | "println" | "print" | "printf" ->
      Some
        (fun context the_module builder fn_name arg_vals ->
          let rec process_arg v =
            let ty = type_of v in
            match classify_type ty with
            | TypeKind.Integer ->
                let bitwidth = integer_bitwidth ty in
                if bitwidth = 1 then
                  let true_str =
                    build_global_stringptr "true" "truestr" builder
                  in
                  let false_str =
                    build_global_stringptr "false" "falsestr" builder
                  in
                  let str_val =
                    build_select v true_str false_str "boolstr" builder
                  in
                  ("%s", [ str_val ])
                else if bitwidth = 8 then ("%c", [ v ])
                else ("%ld", [ v ])
            | TypeKind.Double -> ("%g", [ v ])
            | TypeKind.Pointer -> ("%s", [ v ])
            | TypeKind.Array ->
                let len = array_length ty in
                let fmt_acc = ref [] in
                let val_acc = ref [] in

                for i = 0 to len - 1 do
                  let elem = build_extractvalue v i "exttmp" builder in
                  let e_fmt, e_vals = process_arg elem in
                  fmt_acc := e_fmt :: !fmt_acc;
                  val_acc := !val_acc @ e_vals
                done;

                let fmt_str =
                  "[" ^ String.concat ", " (List.rev !fmt_acc) ^ "]"
                in
                (fmt_str, !val_acc)
            | _ -> ("(complex_type)", [])
          in

          let fmts, vals =
            List.fold_left
              (fun (f_acc, v_acc) v ->
                let f, vs = process_arg v in
                (f :: f_acc, v_acc @ vs))
              ([], []) arg_vals
          in

          let space_sep = String.concat " " (List.rev fmts) in
          let final_fmt =
            if fn_name = "println" then space_sep ^ "\n" else space_sep
          in
          let fmt_val = build_global_stringptr final_fmt "fmt" builder in

          let printf_func = get_printf context the_module in
          let printf_ty =
            var_arg_function_type (i32_type context) [| pointer_type context |]
          in

          let printf_args = Array.of_list (fmt_val :: vals) in
          build_call printf_ty printf_func printf_args "printftmp" builder)
  | _ -> None
