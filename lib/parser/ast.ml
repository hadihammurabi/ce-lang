open Llvm

let ce_ctx = global_context ()
let ce_module = create_module ce_ctx "ce"
let ce_builder = builder ce_ctx

type types =
  | TBool
  | TVoid
  | TString
  | TChar
  | TInt
  | TFloat
  | TUnknown
  | TArray of int * types
[@@deriving show]

let rec t = function
  | TInt -> i64_type ce_ctx
  | TFloat -> double_type ce_ctx
  | TBool -> i1_type ce_ctx
  | TString -> pointer_type ce_ctx
  | TChar -> i8_type ce_ctx
  | TVoid -> void_type ce_ctx
  | TArray (n, inner) -> array_type (t inner) n
  | _ -> failwith "Codegen error: TypeUnknown reached LLVM backend"

type expr =
  | Void
  | String of string
  | Char of char
  | Bool of bool
  | Int of int
  | Float of float
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | Eq of expr * expr
  | Lt of expr * expr
  | Lte of expr * expr
  | Gt of expr * expr
  | Gte of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Neg of expr
  | Call of string * expr list
  | Let of string
  | Array of int * types * expr list
  (* if   cond,  block,      elif cond,  block,            block) *)
  | If of expr * stmt list * (expr * stmt list) list * stmt list option
[@@deriving show]

and param = { name : string; ty : types }

and stmt =
  | Expr of expr
  | DefFN of string * param list * types * stmt list
  | DefLet of string * bool * types * expr
  | Assign of string * expr
  | Return of expr
  | Block of stmt list
  | For of stmt list
  | Break
  | Import of string list

let rec to_string = function
  | Void -> ""
  | String s -> s
  | Char c -> Printf.sprintf "'%c'" c
  | Bool b -> string_of_bool b
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | Let name -> name
  | Add (l, r) -> Printf.sprintf "(%s + %s)" (to_string l) (to_string r)
  | Sub (l, r) -> Printf.sprintf "(%s - %s)" (to_string l) (to_string r)
  | Mul (l, r) -> Printf.sprintf "(%s * %s)" (to_string l) (to_string r)
  | Div (l, r) -> Printf.sprintf "(%s / %s)" (to_string l) (to_string r)
  | Mod (l, r) -> Printf.sprintf "(%s %% %s)" (to_string l) (to_string r)
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
