type expr =
  | Int    of int
  | Float  of float
  | String of string
  | Add    of expr * expr
  | Sub    of expr * expr
  | Mul    of expr * expr
  | Div    of expr * expr
  | Neg    of expr
  | Call   of string * expr list   (* fn_name, args *)

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

let rec eval = function
  | Int n        -> float_of_int n
  | Float f      -> f
  | String _     -> failwith "Cannot eval string as number"
  | Add (l, r)   -> eval l +. eval r
  | Sub (l, r)   -> eval l -. eval r
  | Mul (l, r)   -> eval l *. eval r
  | Div (l, r)   ->
    let d = eval r in
    if d = 0.0 then failwith "Division by zero"
    else eval l /. d
  | Neg e        -> -. (eval e)
  | Call _       -> failwith "Cannot eval call as number"

(* Statement: top-level things that can be executed *)
type stmt =
  | Expr of expr

let exec = function
  | Expr (Call ("println", args)) ->
    let parts = List.map to_string args in
    print_endline (String.concat " " parts)
  | Expr (Call ("print", args)) ->
    let parts = List.map to_string args in
    Printf.printf "%s" (String.concat " " parts)
  | Expr _ -> ()
