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

let rec get_expr expr = 
  match expr with
    | Factor a ->  (get_factor a)
    | Expr (lhs, op, rhs) -> "(" ^ get_op op ^ " " ^ get_expr lhs ^ " " ^ get_expr rhs ^ ")"

let print_expr expr = 
  print_endline (get_expr expr)