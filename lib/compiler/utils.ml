open Llvm
open Ce_parser.Ast
open State

let build_numeric_op lv rv build_int build_float name =
  if type_of lv = double_type ce_ctx then build_float lv rv name ce_builder
  else build_int lv rv name ce_builder

let resolve_property_ptr current_ptr current_ty props =
  let rec get_gep ptr ty props =
    match props with
    | [] -> ptr
    | prop :: rest -> (
        match classify_type ty with
        | TypeKind.Struct ->
            let s_name = Option.get (struct_name ty) in
            let clean_name =
              if String.starts_with ~prefix:"struct." s_name then
                String.sub s_name 7 (String.length s_name - 7)
              else s_name
            in
            let _, field_map = Hashtbl.find struct_registry clean_name in
            let idx = List.assoc prop field_map in
            let next_ptr = build_struct_gep ty ptr idx "prop_ptr" ce_builder in
            let next_ty = (struct_element_types ty).(idx) in
            get_gep next_ptr next_ty rest
        | _ ->
            raise
              (Error ("Cannot access property '" ^ prop ^ "' on non-struct")))
  in
  get_gep current_ptr current_ty props
