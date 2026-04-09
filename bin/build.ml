open Ce_compiler
open Ce_parser
open Ce_lexer
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

let remove_extension filename =
  match String.rindex_opt filename '.' with
  | Some dot_index -> String.sub filename 0 dot_index
  | None -> filename

let rec process_file visited filepath = parse @@ read filepath

let execute file =
  let binary_name = remove_extension file in
  let visited = Hashtbl.create 10 in
  let ast = process_file visited file in
  let _ = ast |> Compiler.compile |> Compiler.export binary_name in
  Printf.printf "Compiled and linked: %s\n" binary_name

let command =
  let doc = "Compile inserted ce-lang code file to binary executable" in
  let info = Cmd.info "build" ~doc in
  Cmd.v info Term.(const execute $ Command.file_arg)
