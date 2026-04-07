open Ce_compiler
open Ce_parser
open Ce_lexer
open Ce_linker
open Cmdliner

let read file = In_channel.with_open_text file In_channel.input_all

let parse src =
  let lexbuf = Lexing.from_string src in
  try Parser.prog Lexer.tokenize lexbuf
  with Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    let line = pos.pos_lnum in
    let col = pos.pos_cnum - pos.pos_bol in
    let token = Lexing.lexeme lexbuf in
    raise
      (Failure
         (Printf.sprintf "Parse error at line %d, column %d, near token '%s'"
            line col token))

let format_position pos =
  let line = pos.Lexing.pos_lnum in
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Printf.sprintf "line %d, column %d" line col

let remove_extension filename =
  match String.rindex_opt filename '.' with
  | Some dot_index -> String.sub filename 0 dot_index
  | None -> filename

let path_of_module_path parts =
  let rec build_path = function
    | [] -> failwith "Empty import path"
    | [file] -> file ^ ".ce"
    | dir :: rest -> Filename.concat dir (build_path rest)
  in
  build_path parts

let rec prefix_expr exports prefix = function
  | Ce_parser.Ast.Add (l, r) -> Ce_parser.Ast.Add (prefix_expr exports prefix l, prefix_expr exports prefix r)
  | Ce_parser.Ast.Sub (l, r) -> Ce_parser.Ast.Sub (prefix_expr exports prefix l, prefix_expr exports prefix r)
  | Ce_parser.Ast.Mul (l, r) -> Ce_parser.Ast.Mul (prefix_expr exports prefix l, prefix_expr exports prefix r)
  | Ce_parser.Ast.Div (l, r) -> Ce_parser.Ast.Div (prefix_expr exports prefix l, prefix_expr exports prefix r)
  | Ce_parser.Ast.Eq (l, r) -> Ce_parser.Ast.Eq (prefix_expr exports prefix l, prefix_expr exports prefix r)
  | Ce_parser.Ast.Lt (l, r) -> Ce_parser.Ast.Lt (prefix_expr exports prefix l, prefix_expr exports prefix r)
  | Ce_parser.Ast.Lte (l, r) -> Ce_parser.Ast.Lte (prefix_expr exports prefix l, prefix_expr exports prefix r)
  | Ce_parser.Ast.Gt (l, r) -> Ce_parser.Ast.Gt (prefix_expr exports prefix l, prefix_expr exports prefix r)
  | Ce_parser.Ast.Gte (l, r) -> Ce_parser.Ast.Gte (prefix_expr exports prefix l, prefix_expr exports prefix r)
  | Ce_parser.Ast.And (l, r) -> Ce_parser.Ast.And (prefix_expr exports prefix l, prefix_expr exports prefix r)
  | Ce_parser.Ast.Or (l, r) -> Ce_parser.Ast.Or (prefix_expr exports prefix l, prefix_expr exports prefix r)
  | Ce_parser.Ast.Neg e -> Ce_parser.Ast.Neg (prefix_expr exports prefix e)
  | Ce_parser.Ast.Call (name, args) ->
      let new_name = if Hashtbl.mem exports name then prefix ^ name else name in
      Ce_parser.Ast.Call (new_name, List.map (prefix_expr exports prefix) args)
  | Ce_parser.Ast.Let name ->
      let new_name = if Hashtbl.mem exports name then prefix ^ name else name in
      Ce_parser.Ast.Let new_name
  | Ce_parser.Ast.Array (n, ty, elems) ->
      Ce_parser.Ast.Array (n, ty, List.map (prefix_expr exports prefix) elems)
  | Ce_parser.Ast.If (cond, then_body, elifs, else_body) ->
      let p_stmt = prefix_stmt exports prefix in
      Ce_parser.Ast.If (
        prefix_expr exports prefix cond,
        List.map p_stmt then_body,
        List.map (fun (c, b) -> (prefix_expr exports prefix c, List.map p_stmt b)) elifs,
        Option.map (List.map p_stmt) else_body
      )
  | e -> e

and prefix_stmt exports prefix = function
  | Ce_parser.Ast.Expr e -> Ce_parser.Ast.Expr (prefix_expr exports prefix e)
  | Ce_parser.Ast.DefFN (name, params, ty, body) ->
      Ce_parser.Ast.DefFN (prefix ^ name, params, ty, List.map (prefix_stmt exports prefix) body)
  | Ce_parser.Ast.DefLet (name, mut, ty, expr) ->
      Ce_parser.Ast.DefLet (prefix ^ name, mut, ty, prefix_expr exports prefix expr)
  | Ce_parser.Ast.Assign (name, expr) ->
      let new_name = if Hashtbl.mem exports name then prefix ^ name else name in
      Ce_parser.Ast.Assign (new_name, prefix_expr exports prefix expr)
  | Ce_parser.Ast.Return e -> Ce_parser.Ast.Return (prefix_expr exports prefix e)
  | Ce_parser.Ast.Block body -> Ce_parser.Ast.Block (List.map (prefix_stmt exports prefix) body)
  | Ce_parser.Ast.For body -> Ce_parser.Ast.For (List.map (prefix_stmt exports prefix) body)
  | stmt -> stmt

let namespace_ast prefix stmts =
  let exports = Hashtbl.create 10 in
  List.iter (fun stmt ->
    match stmt with
    | Ce_parser.Ast.DefFN (name, _, _, _) -> Hashtbl.add exports name true
    | Ce_parser.Ast.DefLet (name, _, _, _) -> Hashtbl.add exports name true
    | _ -> ()
  ) stmts;
  List.map (prefix_stmt exports prefix) stmts

(* Update the parse_file function we made previously to trigger namespacing *)
let rec parse_file visited filepath =
  let abs_path = if Filename.is_relative filepath then filepath else filepath in
  
  if Hashtbl.mem visited abs_path then []
  else begin
    Hashtbl.add visited abs_path true;
    let src = read abs_path in
    let lexbuf = Lexing.from_string src in
    let ast = 
      try Parser.prog Lexer.tokenize lexbuf
      with Parser.Error ->
        let pos = lexbuf.lex_curr_p in
        raise (Failure (Printf.sprintf "Parse error in %s at line %d, column %d" 
          abs_path pos.pos_lnum (pos.pos_cnum - pos.pos_bol)))
    in
    
    let dir = Filename.dirname abs_path in
    
    List.concat_map (fun stmt ->
      match stmt with
      | Ce_parser.Ast.Import path_parts ->
          let relative_target = path_of_module_path path_parts in
          let target_file = Filename.concat dir relative_target in
          let imported_ast = parse_file visited target_file in
          (* Calculate prefix (e.g. "math." or "http.client.") and apply it *)
          let prefix = String.concat "." path_parts ^ "." in
          namespace_ast prefix imported_ast
      | other_stmt -> [other_stmt]
    ) ast
  end

let compile code =
  try code |> parse |> Compiler.compile with
  | Lexer.Lexer_error (msg, pos) ->
      Printf.printf "Lexer error at %s: %s\n\n" (format_position pos) msg;
      exit 1
  | Failure msg ->
      Printf.printf "Error: %s\n\n" msg;
      exit 1

let execute file =
  let binary_file = remove_extension file in
  let _ = file |> read |> compile |> Linker.export binary_file in
  Printf.printf "Compiled and linked: %s\n" binary_file

let command =
  let doc = "Compile inserted ce-lang code file to binary executable" in
  let info = Cmd.info "build" ~doc in
  Cmd.v info Term.(const execute $ Command.file_arg)
