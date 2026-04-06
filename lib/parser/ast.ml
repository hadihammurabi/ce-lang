type expr =
  | Int    of int
  | Float  of float
  | String of string
  | Add    of expr * expr
  | Sub    of expr * expr
  | Mul    of expr * expr
  | Div    of expr * expr
  | Neg    of expr
  | Call   of string * expr list 

let rec to_string = function
  | Int n        -> string_of_int n
  | Float f      -> string_of_float f
  | String s     -> s
  | Add (l, r)   -> Printf.sprintf "(%s + %s)" (to_string l) (to_string r)
  | Sub (l, r)   -> Printf.sprintf "(%s - %s)" (to_string l) (to_string r)
  | Mul (l, r)   -> Printf.sprintf "(%s * %s)" (to_string l) (to_string r)
  | Div (l, r)   -> Printf.sprintf "(%s / %s)" (to_string l) (to_string r)
  | Neg e        -> Printf.sprintf "(-%s)" (to_string e)
  | Call (f, args) ->
    Printf.sprintf "%s(%s)" f (String.concat ", " (List.map to_string args))

type stmt =
  | Expr of expr
  | DefFN of string * expr list

let exec = function
  | DefFN (name, _body) -> ()
  | Expr _ -> ()
