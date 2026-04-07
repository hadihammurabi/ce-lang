type types =
  | TypeBool
  | TypeVoid
  | TypeString
  | TypeInt
  | TypeFloat
  | TypeArray of int * types
[@@deriving show]

type param = { name : string; ty : types (* data type *) }

type expr =
  | String of string
  | Bool of bool
  | Int of int
  | Float of float
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Eq of expr * expr
  | Lt of expr * expr
  | Lte of expr * expr
  | Gt of expr * expr
  | Gte of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Neg of expr
  | Call of string * expr list
  | Var of string
  | Array of int * types * expr list
  (* if   cond,  block,      elif cond,  block,            block) *)
  | If of expr * stmt list * (expr * stmt list) list * stmt list option

and stmt =
  | Expr of expr
  | DefFN of string * param list * types * stmt list
  | DefVar of string * types * expr
  | Return of expr
  | Block of stmt list

let rec to_string = function
  | String s -> s
  | Bool b -> string_of_bool b
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | Var name -> name
  | Add (l, r) -> Printf.sprintf "(%s + %s)" (to_string l) (to_string r)
  | Sub (l, r) -> Printf.sprintf "(%s - %s)" (to_string l) (to_string r)
  | Mul (l, r) -> Printf.sprintf "(%s * %s)" (to_string l) (to_string r)
  | Div (l, r) -> Printf.sprintf "(%s / %s)" (to_string l) (to_string r)
  | Eq (l, r) -> Printf.sprintf "(%s == %s)" (to_string l) (to_string r)
  | Lt (l, r) -> Printf.sprintf "(%s < %s)" (to_string l) (to_string r)
  | Lte (l, r) -> Printf.sprintf "(%s <= %s)" (to_string l) (to_string r)
  | Gt (l, r) -> Printf.sprintf "(%s > %s)" (to_string l) (to_string r)
  | Gte (l, r) -> Printf.sprintf "(%s >= %s)" (to_string l) (to_string r)
  | And (l, r) -> Printf.sprintf "(%s && %s)" (to_string l) (to_string r)
  | Or (l, r) -> Printf.sprintf "(%s || %s)" (to_string l) (to_string r)
  | Neg e -> Printf.sprintf "(-%s)" (to_string e)
  | Call (f, args) ->
      Printf.sprintf "%s(%s)" f (String.concat ", " (List.map to_string args))
  | Array (n, t, elems) ->
      Printf.sprintf "[%d]%s{%s}" n (show_types t)
        (String.concat ", " (List.map to_string elems))
  | If (cond, _, _, _) -> Printf.sprintf "if(%s)" (to_string cond)

let exec = function
  | Expr _ -> ()
  | DefFN (name, params, ty, _body) -> ()
  | DefVar (name, ty, value) -> ()
  | Return _ -> ()
  | Block _ -> ()
