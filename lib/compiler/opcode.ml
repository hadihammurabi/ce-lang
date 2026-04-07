open Ce_parser

type opcode =
  | Push_int    of int
  | Push_float  of float
  | Push_string of string
  | Push_array of int * Ast.types

  | Add
  | Sub
  | Mul
  | Div
  | Neg
  | Return

  | DefVar of string
  | Var of string
  | DefFN of string * Ast.param list
  | Call of string * int
  | LoadParam of string
  | Pop   
  | Halt 

let to_string = function
  | Push_int n    -> Printf.sprintf "PUSH_INT    %d"    n
  | Push_float f  -> Printf.sprintf "PUSH_FLOAT  %g"    f
  | Push_string s -> Printf.sprintf "PUSH_STRING %S"    s
  | Push_array (n, _) -> Printf.sprintf "PUSH_ARRAY  [%d]" n
  | Add           -> "ADD"
  | Sub           -> "SUB"
  | Mul           -> "MUL"
  | Div           -> "DIV"
  | Neg           -> "NEG"
  | Return        -> "RETURN"
  | DefVar name   -> Printf.sprintf "DEF_VAR     %s"   name
  | Var name      -> Printf.sprintf "LOAD_VAR    %s"   name
  | DefFN (name, _)    -> Printf.sprintf "DEF_FN      %s"   name
  | Call (f, n)   -> Printf.sprintf "CALL        %s/%d" f n
  | LoadParam name     -> Printf.sprintf "LOAD_PARAM  %s" name
  | Pop           -> "POP"
  | Halt          -> "HALT"

type program = {
  code: opcode array;
  functions : (string * Ast.param list * Ast.types * Ast.stmt list) list;
  globals : (string * Ast.types * Ast.expr) list;
} 

let dump (code: opcode array) =
  Array.iteri (fun i op ->
    Printf.printf "%04d  %s\n" i (to_string op)
  ) code
