open Parser

let get_factor a = 
  match a with 
    | IntFactor x -> (string_of_int x)
    | IdentFactor {identifier= x} -> x
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
    | FunctionCallExpr {fn_name;  args_list} -> let args_list_str = List.fold_left (fun acc x -> acc ^ " " ^ x) "" (List.map get_expr args_list) in 
                                                      "(() " ^ fn_name.identifier ^ args_list_str ^ ")"

let print_expr expr = 
  print_endline (get_expr expr)

let get_identifier ident = 
  match ident with
    | { identifier: string } -> identifier  

let rec get_statement statement = 
  match statement with 
    | IfStatement {condition; then_branch; _} -> get_expr condition ^ get_statement_list then_branch
    | VariableAssignStatement {var_name; value} -> get_identifier var_name ^ " = " ^ get_expr value
    | Expression expr -> get_expr expr


and get_statement_list stmt_list = 
  match stmt_list with 
    | statement :: rest_of_statements -> get_statement statement ^ get_statement_list rest_of_statements
    | [] -> "EOF"