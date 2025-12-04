open Parser

let get_factor a = 
  match a with 
    | IntFactor x -> (string_of_int x)
    | IdentFactor x -> x
let get_op op = 
  match op with 
    | Multiply -> "*"
    | Divide -> "/"
    | Plus -> "+"
    | Minus -> "-"

let get_prefix_op op = 
  match op with
    | Increment -> "++"
    | Decrement -> "--"

let rec get_expr expr = 
  match expr with
    | Factor a ->  (get_factor a)
    | InfixExpr (lhs, op, rhs) -> "(" ^ get_op op ^ " " ^ get_expr lhs ^ " " ^ get_expr rhs ^ ")"
    | PrefixExpr (op, rhs) -> "(" ^ get_prefix_op op ^ " " ^ get_expr rhs ^ ")"
    | _ -> "Here"

let print_expr expr = 
  print_endline (get_expr expr)