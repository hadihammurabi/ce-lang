{
  open Ce_parser.Parser

  exception Lexer_error of string * Lexing.position
}

let digit      = ['0'-'9']
let alpha      = ['a'-'z' 'A'-'Z' '_']
let alnum      = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let whitespace = [' ' '\t' '\r']

rule tokenize = parse
  | whitespace+        { tokenize lexbuf }
  | '\n'               { NEWLINE }

  | digit+ '.' digit+  { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | digit+             { INT (int_of_string (Lexing.lexeme lexbuf)) }

  | "fn"               { FN }
  | alpha alnum*       { IDENT (Lexing.lexeme lexbuf) }

  | '"'                { let buf = Buffer.create 32 in string_lit buf lexbuf }

  | '+'                { PLUS }
  | '-'                { MINUS }
  | '*'                { STAR }
  | '/'                { SLASH }
  | '('                { LPAREN }
  | ')'                { RPAREN }
  | '{'                { LBRACE }
  | '}'                { RBRACE }
  | ','                { COMMA }

  | eof                { EOF }
  | _ as c             { raise (Lexer_error (Printf.sprintf "Unexpected character: '%c'" c, lexbuf.lex_curr_p)) }

and string_lit buf = parse
  | '"'        { STRING (Buffer.contents buf) }
  | '\\' 'n'   { Buffer.add_char buf '\n'; string_lit buf lexbuf }
  | '\\' 't'   { Buffer.add_char buf '\t'; string_lit buf lexbuf }
  | '\\' '\\'  { Buffer.add_char buf '\\'; string_lit buf lexbuf }
  | '\\' '"'   { Buffer.add_char buf '"';  string_lit buf lexbuf }
  | '\n'       { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; string_lit buf lexbuf }
  | eof        { raise (Lexer_error ("Unterminated string", lexbuf.lex_curr_p)) }
  | _ as c     { Buffer.add_char buf c;    string_lit buf lexbuf }
