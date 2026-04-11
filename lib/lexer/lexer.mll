{
  open Ce_parser.Parser

  exception Lexer_error of string * Lexing.position

  let last_token_was_end_of_expr = ref false
  let nesting_level = ref 0

  let expr_end tok = last_token_was_end_of_expr := true; tok
  let expr_cont tok = last_token_was_end_of_expr := false; tok
  let open_paren tok = incr nesting_level; expr_cont tok
  let close_paren tok = decr nesting_level; expr_end tok
}

let digit      = ['0'-'9']
let alpha      = ['a'-'z' 'A'-'Z' '_']
let alnum      = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let whitespace = [' ' '\t' '\r']

rule tokenize = parse
  | whitespace+        { tokenize lexbuf }
  
  | '\n'               { 
      Lexing.new_line lexbuf;
      if !last_token_was_end_of_expr && !nesting_level = 0 then (
        last_token_was_end_of_expr := false;
        NEWLINE
      ) else (
        tokenize lexbuf
      )
    }  

  | digit+ '.' digit+  { expr_end (FLOAT (float_of_string (Lexing.lexeme lexbuf))) }
  | digit+             { expr_end (INT (int_of_string (Lexing.lexeme lexbuf))) }

  | "fn"               { expr_cont FN }
  | "let"              { expr_cont LET }          
  | "mut"              { expr_cont MUT }
  | "if"               { expr_cont IF }
  | "else"             { expr_cont ELSE }
  | "for"              { expr_cont FOR }
  | "break"            { expr_end BREAK }
  | "return"           { expr_end RETURN }
  | "bool"             { expr_end TYPE_BOOL }     
  | "true"             { expr_end TRUE }     
  | "false"            { expr_end FALSE }     
  | "string"           { expr_end TYPE_STRING }     
  | "int"              { expr_end TYPE_INT }     
  | "float"            { expr_end TYPE_FLOAT }   
  | "char"             { expr_end TYPE_CHAR }
  | "void"             { expr_end TYPE_VOID }   
  | "import"           { expr_cont IMPORT }
  | "type"             { expr_cont TYPE }
  | "impl"             { expr_cont IMPL }
  | "raise"            { expr_cont RAISE }
  | "catch"            { expr_cont CATCH }
  | "struct"           { expr_cont STRUCT }
  | "interface"        { expr_cont INTERFACE }
  | alpha alnum* { expr_end (IDENT (Lexing.lexeme lexbuf)) }

  | '"'                { 
      let buf = Buffer.create 32 in 
      let s = string_lit buf lexbuf in
      expr_end (STRING s)
    }

  | '\'' ([^ '\\' '\''] as c) '\'' { expr_end (CHAR c) }
  | '\'' '\\' 'n' '\''             { expr_end (CHAR '\n') }
  | '\'' '\\' 't' '\''             { expr_end (CHAR '\t') }
  | '\'' '\\' '\\' '\''            { expr_end (CHAR '\\') }
  | '\'' '\\' '\'' '\''            { expr_end (CHAR '\'') }

  | '+'                { expr_cont PLUS }
  | '-'                { expr_cont MINUS }
  | '*'                { expr_cont STAR }
  | '/'                { expr_cont SLASH }
  | '%'                { expr_cont MOD }
  | '('                { open_paren LPAREN }
  | ')'                { close_paren RPAREN }
  | '{'                { expr_cont LBRACE }
  | '}'                { expr_end RBRACE }
  | '['                { open_paren LBRACKET }
  | ']'                { close_paren RBRACKET }
  | ','                { expr_cont COMMA }
  | '='                { expr_cont EQUALS } 
  | "."                { expr_cont DOT }
  | "&"                { expr_cont AMP }
  | ";"                { expr_cont SEMICOLON }
  | "!"                { expr_cont BANG }

  | "=="               { expr_cont EQEQ }
  | "<="               { expr_cont LTE }
  | ">="               { expr_cont GTE }
  | "<"                { expr_cont LT }
  | ">"                { expr_cont GT }
  | "&&"               { expr_cont AND }
  | "||"               { expr_cont OR }

  | eof                { EOF }
  | _ as c             { raise (Lexer_error (Printf.sprintf "Unexpected character: '%c'" c, lexbuf.lex_curr_p)) }

and string_lit buf = parse
  | '"'        { Buffer.contents buf }
  | '\\' 'n'   { Buffer.add_char buf '\n'; string_lit buf lexbuf }
  | '\\' 't'   { Buffer.add_char buf '\t'; string_lit buf lexbuf }
  | '\\' '\\'  { Buffer.add_char buf '\\'; string_lit buf lexbuf }
  | '\\' '"'   { Buffer.add_char buf '"';  string_lit buf lexbuf }
  | '\n'       { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; string_lit buf lexbuf }
  | eof        { raise (Lexer_error ("Unterminated string", lexbuf.lex_curr_p)) }
  | _ as c     { Buffer.add_char buf c;    string_lit buf lexbuf }
