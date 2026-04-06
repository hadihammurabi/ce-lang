open Opcode

type value = VInt of int | VFloat of float | VString of string | VUnit

exception Vm_error of string
let err fmt = Printf.ksprintf (fun s -> raise (Vm_error s)) fmt

let to_str = function
  | VInt n   -> string_of_int n
  | VFloat f -> string_of_float f
  | VString s -> s
  | VUnit    -> "()"

let arith op_i op_f l r = match l, r with
  | VInt a,   VInt b   -> VInt   (op_i a b)
  | VFloat a, VFloat b -> VFloat (op_f a b)
  | VInt a,   VFloat b -> VFloat (op_f (float_of_int a) b)
  | VFloat a, VInt b   -> VFloat (op_f a (float_of_int b))
  | _ -> err "arithmetic requires numeric operands"

let builtin name args = match name, args with
  | "println", _ -> print_endline (String.concat " " (List.map to_str args)); VUnit
  | "print",   _ -> print_string  (String.concat " " (List.map to_str args)); VUnit
  | "int",   [VFloat f]  -> VInt (int_of_float f)
  | "int",   [VInt n]    -> VInt n
  | "int",   [VString s] -> (try VInt (int_of_string s)   with _ -> err "int(): bad value %S" s)
  | "float", [VInt n]    -> VFloat (float_of_int n)
  | "float", [VFloat f]  -> VFloat f
  | "float", [VString s] -> (try VFloat (float_of_string s) with _ -> err "float(): bad value %S" s)
  | "str",   [v]         -> VString (to_str v)
  | _ -> err "Unknown function: %s" name

let execute prog =
  let stack = Stack.create () in
  let ip    = ref 0 in
  let pop () = if Stack.is_empty stack then err "stack underflow" else Stack.pop stack in
  while prog.(!ip) <> Halt do
    let instr = prog.(!ip) in
    incr ip;
    match instr with
    | Push_int n    -> Stack.push (VInt n)    stack
    | Push_float f  -> Stack.push (VFloat f)  stack
    | Push_string s -> Stack.push (VString s) stack
    | Add -> let r = pop () and l = pop () in
      Stack.push (match l, r with
        | VString a, v -> VString (a ^ to_str v)
        | v, VString b -> VString (to_str v ^ b)
        | _ -> arith ( + ) ( +. ) l r) stack
    | Sub -> let r = pop () and l = pop () in Stack.push (arith ( - ) ( -. ) l r) stack
    | Mul -> let r = pop () and l = pop () in Stack.push (arith ( * ) ( *. ) l r) stack
    | Div -> let r = pop () and l = pop () in
      (match r with VInt 0 | VFloat 0.0 -> err "division by zero" | _ -> ());
      Stack.push (arith ( / ) ( /. ) l r) stack
    | Neg -> (match pop () with
      | VInt n   -> Stack.push (VInt (- n))    stack
      | VFloat f -> Stack.push (VFloat (-. f)) stack
      | _ -> err "negation requires a numeric operand")
    | Call (name, argc) ->
      let args = List.rev (List.init argc (fun _ -> pop ())) in
      Stack.push (builtin name args) stack
    | Pop  -> ignore (pop ())
    | Halt -> ()
    | _ -> ()
  done
