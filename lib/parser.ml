open Lexer

type t = {
  current: Lexer.token;
  peek: Lexer.token;
}

type op = 
  | Multiply
  | Divide
  | Plus
  | Minus


type node = 
  | Statement of statement
  | Expression of expr

and statement = 
  | IfStatement
  | VariableStatement

and expr =
  | Factor of factor
  | InfixExpr of expr * op * expr
  | PrefixExpr of op * expr
  | PostfixExpr of expr * op

and factor =
  | IntFactor of int
  | IdentFactor of string

let init = 
  let curr = next_token ()
    in 
    let next = next_token () in
      {current = curr; peek = next}

let advance parser =
  {current = parser.peek; peek = next_token()}


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

(* ( a + b) * c + d*)
let rec parse_prefix_expr parser = 
  match parser.current with 
  | Identifier (name) -> Ok (Factor (IdentFactor (name)), parser)
  | Integer (value) -> Ok (Factor (IntFactor (value)), parser)
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

let parse () = 
  let parser = init in
    let* lhs, _ = (parse_expr parser 0.0) in  Ok lhs


