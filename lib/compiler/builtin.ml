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
          let fmt_parts = ref [] in

          let processed_args =
            List.map
              (fun v ->
                let ty = type_of v in
                if ty = i64_type context then (
                  fmt_parts := "%ld" :: !fmt_parts;
                  v)
                else if ty = double_type context then (
                  fmt_parts := "%g" :: !fmt_parts;
                  v)
                else if ty = pointer_type context then (
                  fmt_parts := "%s" :: !fmt_parts;
                  v)
                else if ty = i1_type context then (
                  fmt_parts := "%s" :: !fmt_parts;
                  let true_str =
                    build_global_stringptr "true" "truestr" builder
                  in
                  let false_str =
                    build_global_stringptr "false" "falsestr" builder
                  in
                  build_select v true_str false_str "boolstr" builder)
                else (
                  fmt_parts := "(complex_type)" :: !fmt_parts;
                  v))
              arg_vals
          in

          let space_sep = String.concat " " (List.rev !fmt_parts) in
          let final_fmt =
            if fn_name = "println" then space_sep ^ "\n" else space_sep
          in
          let fmt_val = build_global_stringptr final_fmt "fmt" builder in

          let printf_func = get_printf context the_module in
          let printf_ty =
            var_arg_function_type (i32_type context) [| pointer_type context |]
          in
          let printf_args = Array.of_list (fmt_val :: processed_args) in
          build_call printf_ty printf_func printf_args "printftmp" builder)
  | _ -> None
