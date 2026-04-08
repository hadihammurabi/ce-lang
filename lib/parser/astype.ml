open Ast

let void = { ty = TypeVoid; kind = Void }
let unknown x = { ty = TypeUnknown; kind = x }
let string x = { ty = TypeString; kind = String x }
let bool x = { ty = TypeBool; kind = Bool x }
let int x = { ty = TypeInt; kind = Int x }
let float x = { ty = TypeFloat; kind = Float x }
