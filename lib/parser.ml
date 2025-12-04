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

type prefix_op =
  | Increment
  | Decrement


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
  | PostfixExpr of expr * prefix_op

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


let parse_op parser = 
  match parser.current with
    | Lexer.Multiply -> Ok (Multiply, 2.0, 2.1)
    | Lexer.Divide -> Ok (Divide, 2.0, 2.1) 
    | Lexer.Plus -> Ok (Plus, 1.0, 1.1)
    | Lexer.Minus -> Ok (Minus, 1.0, 1.1)
    | _ -> Error ("Error parsing operation: " ^ token_to_string parser.current)
  
let peek_is parser tok =
  if parser.peek = tok then true else false

let rec parse_prefix_expr parser = 
  match parser.current with 
  | Identifier (name) -> Ok (Factor (IdentFactor (name)), parser)
  | Integer (value) -> Ok (Factor (IntFactor (value)), parser)
  | Increment -> let parser = advance parser in
                  let* rhs, parser = (parse_expr parser 8.0) in
                    let prefix_expr = PrefixExpr ( Increment , rhs) in Ok (prefix_expr, parser)
  | OpenRoundBracket -> let parser' = advance parser in 
                          let* expr, parser = (parse_expr parser' 0.0) in 
                            if peek_is parser CloseRoundBracket then Ok (expr, advance parser) else Error "No closing bracket"
  | _ -> Error ("Error expected factor, didn't find one. Token: " ^ (Lexer.token_to_string parser.current))

and parse_expr parser min_bp = 
  let* lhs, parser = parse_prefix_expr parser in
  if peek_is parser Semicolon || peek_is parser CloseRoundBracket then Ok (lhs, parser)
  else 
    let parser = advance parser in
    let rec parse_infix_op lhs' parser = 
      if peek_is parser Semicolon || peek_is parser CloseRoundBracket then Ok (lhs', parser)
      else
        let* op, lbp, rbp = (parse_op parser) in
        if  lbp < min_bp then  Ok (lhs', parser) else 
        let parser = advance parser in
         let* rhs, parser = parse_expr parser rbp in
          (parse_infix_op (InfixExpr (lhs', op, rhs)) parser) in 
           (parse_infix_op lhs parser)

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


