open Lexer

type t = {
  current: Lexer.token;
  peek: Lexer.token;
  lexer: Lexer.lexer;
}

type infix_op = 
  | Multiply
  | Divide
  | Plus
  | Minus
  | ArrayIndex
  | FunctionCall

and prefix_op =
  | Increment
  | Decrement
  | Minus
  | Plus

and postfix_op = 
  | Bang
  | Increment
  | Decrement
  | FunctionCall
  | ArrayIndex


type node = 
  | Statement of statement
  | Expression of expr

and statement = 
  | IfStatement
  | FunctionDefinition
  | VariableAssignment

and expr =
  | Factor of factor
  | InfixExpr of expr * infix_op * expr
  | PrefixExpr of prefix_op * expr
  | PostfixExpr of expr * postfix_op
  | FunctionCallExpr of {fn_name: string; args_list: expr list}

and factor =
  | IntFactor of int
  | IdentFactor of string

let init lexer = 
  let lex_state, curr = next_token lexer
    in 
      let peek_lex_state, next_token = (next_token lex_state) in
        {current = curr; peek = next_token; lexer = peek_lex_state}

let advance parser =
  let lex_state, peek = next_token parser.lexer in
    {current = parser.peek; peek = peek; lexer = lex_state}


let ( let* ) res f = Base.Result.bind res ~f


let get_infix_op_bp parser = 
  match parser.peek with
    | Lexer.Multiply -> Some (Multiply, 2.0, 2.1)
    | Lexer.Divide -> Some (Divide, 2.0, 2.1) 
    | Lexer.Plus -> Some (Plus, 1.0, 1.1)
    | Lexer.Minus -> Some (Minus, 1.0, 1.1)
    | _ -> None

let get_prefix_op_bp parser = 
  match parser.current with
    | Increment -> Ok (Increment, 6.0)
    | Decrement -> Ok (Decrement, 6.0)
    | Minus -> Ok (Minus, 3.0)
    | Plus -> Ok (Plus, 3.0)
    | _ -> Error ("Error parsing operation: " ^ token_to_string parser.current)
  
let get_postfix_op_bp parser = 
  match parser.peek with
    | Bang -> Some (Bang, 4.0)
    | Increment -> Some (Increment, 5.0)
    | Decrement -> Some (Decrement, 5.0)
    | OpenRoundBracket -> Some (FunctionCall, 8.0)
    | OpenSquareBracket -> Some (ArrayIndex, 9.0)
    | _ -> None

(* a++! + 5 *)  
let peek_is parser tok =
  if parser.peek = tok then true else false

let rec parse_prefix_expr parser = 
  match parser.current with 
  | Identifier (name) -> Ok (Factor (IdentFactor (name)), parser)
  | Integer (value) -> Ok (Factor (IntFactor (value)), parser)
  | OpenRoundBracket -> let parser' = advance parser in 
                          let* expr, parser = (parse_expr parser' 0.0) in 
                            if peek_is parser CloseRoundBracket then Ok (expr, advance parser) else Error "No closing bracket"
  | _ -> let* op, rbp = get_prefix_op_bp parser in
          let parser = advance parser in
            let* rhs, parser = (parse_expr parser rbp) in
                let prefix_expr = PrefixExpr ( op , rhs) in 
                  Ok (prefix_expr, parser)

and parse_expr parser min_bp = 
  let* lhs, parser = parse_prefix_expr parser in
  if peek_is parser Semicolon then Ok (lhs, parser)
  else 
    let rec parse_infix_op lhs' parser = 
      if peek_is parser Semicolon then Ok (lhs', parser)
      else
        match (get_postfix_op_bp parser) with
        | Some (op, lbp) -> if lbp < min_bp then Ok (lhs', parser) 
                            else 
                              if op = ArrayIndex then 
                                let* rhs, parser = (parse_expr (advance (advance parser)) 0.0) in
                                  let arr_index_expr = (InfixExpr (lhs', ArrayIndex, rhs)) in 
                                    if peek_is parser CloseSquareBracket then (parse_infix_op arr_index_expr (advance parser)) else Error "No closing square bracket"
                              else if op = FunctionCall then 
                                let* fn_name = get_fn_name lhs in
                                let* arg_list, parser = (parse_call_expression (advance (advance parser)) []) in
                                  let function_call_expr = (FunctionCallExpr {fn_name = fn_name; args_list = arg_list}) in 
                                    if peek_is parser CloseRoundBracket then (parse_infix_op function_call_expr (advance parser)) else Error "No closing round bracket"
                                
                              else
                                let postfix_expr = (PostfixExpr (lhs', op)) in 
                                  (parse_infix_op postfix_expr (advance parser))
        | None ->
          match (get_infix_op_bp parser) with
          | Some (op, lbp, rbp) ->
            if  lbp < min_bp then  Ok (lhs', parser) else 
            let* rhs, parser = parse_expr (advance (advance parser)) rbp in (* advance twice, because we use peek to get op's bps*)
              (parse_infix_op (InfixExpr (lhs', op, rhs)) parser) 
          | None -> Ok (lhs', parser) 
            in (parse_infix_op lhs parser)

(* a(x, y)*)

and parse_call_expression parser arg_list =
    let* arg, parser = (parse_expr parser 0.0) in 
      if peek_is parser CommaSeparator then 
       (parse_call_expression (advance (advance parser)) (arg:: arg_list))
    else if peek_is parser CloseRoundBracket then
      let* arg, parser = (parse_expr parser 0.0) in
        Ok (List.rev (arg:: arg_list), parser)
      else Error "Unexpected token"

and get_fn_name node =
  match node with
    | Factor (x) -> get_fn_name_from_factor x
    | _ -> Error "Bad fn name for call"
and get_fn_name_from_factor x =
  match x with
    | IdentFactor (y) -> Ok y
    | _ -> Error ("Bad fn name for call")
      
    

let rec parse_statement parser =
  match parser.current with
    | If -> (parse_if)
    | Fun -> (parse_fun)
    | Identifier (_) -> (parse_variable_assign)
    | _ -> Error ("Could not parse statement")

and parse_if = 
  Ok IfStatement
and parse_fun =
  Ok FunctionDefinition
and parse_variable_assign =
  Ok VariableAssignment


let parse lexer = 
  let parser = init lexer in
    let* lhs, _ = (parse_expr parser 0.0) in  Ok lhs


