open Ce_parser.Ast

let parse src =
  let lexbuf = Lexing.from_string src in
  Ce_parser.Parser.prog Ce_lexer.Lexer.tokenize lexbuf

let run src =
  match parse src with
  | exception Ce_lexer.Lexer.Lexer_error msg ->
    Printf.printf "Lexer error: %s\n\n" msg
  | exception Ce_parser.Parser.Error ->
    Printf.printf "Parser error: syntax error\n\n"
  | exception Failure msg ->
    Printf.printf "Runtime error: %s\n\n" msg
  | stmts ->
    List.iter exec stmts

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: ce <file.ce>\n";
    exit 1
  end;

  let file = Sys.argv.(1) in
  let ic = open_in file in
  let src = really_input_string ic (in_channel_length ic) in
  close_in ic;
  run src
