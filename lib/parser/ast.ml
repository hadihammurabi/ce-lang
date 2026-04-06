type types = TypeInt | TypeFloat

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
  | Var    of string

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
  | Var name     -> name

type stmt =
  | Expr of expr
  | DefFN of string * expr list
  | DefVar of string * types * expr

let exec = function
  | Expr _ -> ()
  | DefFN (name, _body) -> ()
  | DefVar (name, ty, value) -> ()
