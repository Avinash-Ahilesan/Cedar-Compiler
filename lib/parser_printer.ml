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
    | ArrayIndex -> "[]"
    | FunctionCall -> "()"

let get_prefix_op (op: prefix_op) = 
  match op with
    | Increment -> "++"
    | Decrement -> "--"
    | Minus -> "-"
    | Plus -> "+"

let get_postfix_op (op: postfix_op) = 
  match op with
    | Bang -> "!"
    | Increment -> "++"
    | Decrement -> "--"
    | ArrayIndex -> "[]"
    | FunctionCall -> "()"

let rec get_expr expr = 
  match expr with
    | Factor a ->  (get_factor a)
    | InfixExpr (lhs, op, rhs) -> "(" ^ get_op op ^ " " ^ get_expr lhs ^ " " ^ get_expr rhs ^ ")"
    | PrefixExpr (op, rhs) -> "(" ^ get_prefix_op op ^ " " ^ get_expr rhs ^ ")"
    | PostfixExpr (rhs, op) -> "(" ^ get_expr rhs ^ " " ^ get_postfix_op op ^ ")"

let print_expr expr = 
  print_endline (get_expr expr)