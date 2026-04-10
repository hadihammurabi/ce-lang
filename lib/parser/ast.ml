open Llvm

let ce_ctx = global_context ()
let ce_module = create_module ce_ctx "ce"
let ce_builder = builder ce_ctx

type types =
  | TBool
  | TVoid
  | TString
  | TChar
  | TPointer of types
  | TNamed of string
  | TStruct of string
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
  | TPointer _ -> pointer_type ce_ctx
  | TVoid -> void_type ce_ctx
  | TArray (n, inner) -> array_type (t inner) n
  | _ -> failwith "Codegen error: TypeUnknown reached LLVM backend"

type expr =
  | Void
  | String of string
  | Char of char
  | Ref of expr
  | Deref of expr
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
  | ArrayAccess of string * expr
  (* if   cond,  block,      elif cond,  block,            block) *)
  | If of expr * stmt list * (expr * stmt list) list * stmt list option
[@@deriving show]

and param = { param_name : string; ty : types }
and struct_field = { field_name : string; ty : types }

and stmt =
  | Expr of expr
  | DefFN of string * param list * types * stmt list
  | DefLet of string * bool * types * expr
  | DefType of string * types
  | DefStruct of string * struct_field list
  | Assign of string * expr
  | ArrayAssign of string * expr * expr
  | DerefAssign of expr * expr
  | Return of expr
  | Block of stmt list
  | For of stmt list
  | Break
  | Import of string list
