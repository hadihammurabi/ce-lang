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

let format_position pos =
  let line = pos.Lexing.pos_lnum in
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Printf.sprintf "line %d, column %d" line col

let remove_extension filename =
  match String.rindex_opt filename '.' with
  | Some dot_index -> String.sub filename 0 dot_index
  | None -> filename

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
  let _ = file |> read |> compile |> Compiler.export binary_file in
  Printf.printf "Compiled and linked: %s\n" binary_file

let command =
  let doc = "Compile inserted ce-lang code file to binary executable" in
  let info = Cmd.info "build" ~doc in
  Cmd.v info Term.(const execute $ Command.file_arg)
