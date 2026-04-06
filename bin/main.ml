open Ce_vm

let parse src = Ce_parser.Parser.prog Ce_lexer.Lexer.tokenize (Lexing.from_string src)
let read file = In_channel.with_open_text file In_channel.input_all
let run code = 
  try
    code
      |> parse
      |> Compiler.compile
      |> Opcode.dump
  with
  | Ce_lexer.Lexer.Lexer_error msg -> Printf.printf "Lexer error: %s\n\n" msg
  | Ce_parser.Parser.Error -> Printf.printf "Parser error: syntax error\n\n"
  | Failure msg -> Printf.printf "Error: %s\n\n" msg

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: ce <file.ce>\n";
    exit 1
  end;

  run (read Sys.argv.(1))
