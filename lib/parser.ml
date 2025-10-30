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


type factor =
  | IntFactor of int
  | IdentFactor of string

type expr =
  | Factor of factor
  | Expr of expr * op * expr

let init = 
  let curr = next_token ()
    in 
    let next = next_token () in
      {current = curr; peek = next}

let advance parser =
  {current = parser.peek; peek = next_token()}


let ( let* ) res f = Base.Result.bind res ~f


let parse_factor parser = 
  match parser.current with 
  | Identifier (name) -> Ok (Factor (IdentFactor (name)))
  | Integer (value) -> Ok (Factor (IntFactor (value)))
  | _ -> Error ("Error expected factor, didn't find one. Token: " ^ (Lexer.token_to_string parser.current))

let parse_op parser = 
  match parser.current with
    | Lexer.Multiply -> Ok (Multiply, 2.0, 2.1)
    | Lexer.Divide -> Ok (Divide, 2.0, 2.1) 
    | Lexer.Plus -> Ok (Plus, 1.0, 1.1)
    | Lexer.Minus -> Ok ( Minus, 1.0, 1.1)
    | _ -> Error ("Error parsing operation: " ^ token_to_string parser.current)
  
let peek_is parser tok =
  if parser.peek = tok then true else false

(* a + b * c + d*)
let rec parse_expr parser min_bp = 
  let* lhs = parse_factor parser in
  if peek_is parser Semicolon then Ok (lhs, parser)
  else 
    let parser = advance parser in
    let rec process_op lhs' parser = 
      if peek_is parser Semicolon then Ok (lhs', parser)
      else
        let* op, lbp, rbp = (parse_op parser) in
        if  lbp < min_bp then  Ok (lhs', parser) else 
        let parser = advance parser in
         let* rhs, parser = parse_expr parser rbp in
          (process_op (Expr (lhs', op, rhs)) parser) in 
           (process_op lhs parser)



let parse () = 
  let parser = init in
    let* lhs, _ = (parse_expr parser 0.0) in  Ok lhs


