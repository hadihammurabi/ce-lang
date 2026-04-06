open Ce_vm

let read file = In_channel.with_open_text file In_channel.input_all

let parse src =
  let lexbuf = Lexing.from_string src in
  try
    Ce_parser.Parser.prog Ce_lexer.Lexer.tokenize lexbuf
  with Ce_parser.Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    let line = pos.pos_lnum in
    let col = pos.pos_cnum - pos.pos_bol + 1 in
    let token = Lexing.lexeme lexbuf in
    raise (Failure (Printf.sprintf "Parse error at line %d, column %d, near token '%s'" line col token))

let format_position pos =
  let line = pos.Lexing.pos_lnum in
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  Printf.sprintf "line %d, column %d" line col

let run code = 
  try
    code
      |> parse
      |> Compiler.compile
      |> Opcode.dump
  with
  | Ce_lexer.Lexer.Lexer_error (msg, pos) ->
    Printf.printf "Lexer error at %s: %s\n\n" (format_position pos) msg;
    exit 1
  | Failure msg -> Printf.printf "Error: %s\n\n" msg;
    exit 1

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: ce <file.ce>\n";
    exit 1
  end;

  run (read Sys.argv.(1))
