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
  | TResult of types
  | TInt
  | TI8
  | TI16
  | TI32
  | TI64
  | TI128
  | TUInt
  | TU8
  | TU16
  | TU32
  | TU64
  | TU128
  | TFloat
  | TF32
  | TF64
  | TUnknown
  | TGenericParam of string
  | TGenericInst of string * types list
  | TArray of int * types
  | TTuple of types list
[@@deriving show]

let rec t = function
  | TInt | TUInt | TI32 | TU32 -> i32_type ce_ctx
  | TI8 | TU8 -> i8_type ce_ctx
  | TI16 | TU16 -> i16_type ce_ctx
  | TI64 | TU64 -> i64_type ce_ctx
  | TI128 | TU128 -> integer_type ce_ctx 128
  | TFloat | TF64 -> double_type ce_ctx
  | TF32 -> float_type ce_ctx
  | TBool -> i1_type ce_ctx
  | TString -> pointer_type ce_ctx
  | TChar -> i8_type ce_ctx
  | TPointer _ -> pointer_type ce_ctx
  | TVoid -> void_type ce_ctx
  | TArray (n, inner) -> array_type (t inner) n
  | TTuple ts -> struct_type ce_ctx (Array.of_list (List.map t ts))
  | _ -> failwith "Codegen error: TypeUnknown reached LLVM backend"

type expr =
  | Void
  | Nil
  | String of string
  | Char of char
  | Ref of expr
  | Catch of expr * string * types * stmt list
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
  | Not of expr
  | Call of string * types list * expr list
  | Let of string
  | Array of int * types * expr list
  | ArrayAccess of string * expr
  | If of expr * stmt list * (expr * stmt list) list * stmt list option
  | Struct of string * types list * (string * expr) list
  | Tuple of expr list
[@@deriving show]

and param = { param_name : string; ty : types }
and struct_field = { field_name : string; ty : types; is_mut : bool }
and fn_signature = { fn_name : string; params : param list; ret_ty : types }

and stmt =
  | Expr of expr
  | DefFN of string * (string * types) list * param list * types * stmt list
  | DefLet of string * bool * types * expr option
  | DefType of string * types
  | DefStruct of string * (string * types) list * struct_field list
  | Assign of string * expr
  | Impl of
      string
      * (string * types) list
      * (string * string * bool * param list * types * stmt list) list
  | ArrayAssign of string * expr * expr
  | DerefAssign of expr * expr
  | Return of expr
  | Block of stmt list
  | For of stmt option * expr option * stmt option * stmt list
  | Break
  | Import of string list
  | Raise of expr
  | DefInterface of string * fn_signature list
