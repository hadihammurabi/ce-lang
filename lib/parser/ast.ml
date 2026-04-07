type types = 
  | TypeInt 
  | TypeFloat 
  | TypeVoid 
  | TypeArray of int * types
[@@deriving show]

type param = {
  name: string;
  ty: types; (* data type *)
}

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
  | Array  of int * types * expr list

let rec to_string = function
  | Int n        -> string_of_int n
  | Float f      -> string_of_float f
  | String s     -> s
  | Var name     -> name
  | Add (l, r)   -> Printf.sprintf "(%s + %s)" (to_string l) (to_string r)
  | Sub (l, r)   -> Printf.sprintf "(%s - %s)" (to_string l) (to_string r)
  | Mul (l, r)   -> Printf.sprintf "(%s * %s)" (to_string l) (to_string r)
  | Div (l, r)   -> Printf.sprintf "(%s / %s)" (to_string l) (to_string r)
  | Neg e        -> Printf.sprintf "(-%s)" (to_string e)
  | Call (f, args) ->
    Printf.sprintf "%s(%s)" f (String.concat ", " (List.map to_string args))
  | Array(n, t, elems) ->
    Printf.sprintf "[%d]%s{%s}" n (show_types t)
      (String.concat ", " (List.map to_string elems))

type stmt =
  | Expr of expr
  | DefFN of string * param list * types * stmt list
  | DefVar of string * types * expr
  | Return of expr

let exec = function
  | Expr _ -> ()
  | DefFN (name, params, ty, _body) -> ()
  | DefVar (name, ty, value) -> ()
  | Return _ -> ()
