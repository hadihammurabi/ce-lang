type opcode =
  | Push_int    of int
  | Push_float  of float
  | Push_string of string

  | Add
  | Sub
  | Mul
  | Div
  | Neg

  | Call of string * int
  | DefFN of string

  | Pop   
  | Halt 

let to_string = function
  | Push_int n    -> Printf.sprintf "PUSH_INT    %d"    n
  | Push_float f  -> Printf.sprintf "PUSH_FLOAT  %g"    f
  | Push_string s -> Printf.sprintf "PUSH_STRING %S"    s
  | Add           -> "ADD"
  | Sub           -> "SUB"
  | Mul           -> "MUL"
  | Div           -> "DIV"
  | Neg           -> "NEG"
  | Call (f, n)   -> Printf.sprintf "CALL        %s/%d" f n
  | DefFN name    -> Printf.sprintf "DEF_FN      %s"   name
  | Pop           -> "POP"
  | Halt          -> "HALT"

type program = opcode array

let dump (prog : program) =
  Array.iteri (fun i op ->
    Printf.printf "%04d  %s\n" i (to_string op)
  ) prog
