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
  {current = next_token(); peek = next_token()}

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
    | Lexer.Multiply -> Ok (advance parser, Multiply, 2.0, 2.1)
    | Lexer.Divide -> Ok (advance parser, Divide, 2.0, 2.1) 
    | Lexer.Plus -> Ok (advance parser, Plus, 1.0, 1.1)
    | Lexer.Minus -> Ok (advance parser, Minus, 1.0, 1.1)
    | _ -> Error "Error parsing operation"
  
let peek_is parser tok =
  if parser.peek = tok then true else false

let rec parse_expr parser min_bp = 
  let* lhs = parse_factor parser in
  if peek_is parser Semicolon then Ok lhs
  else 
    let parser = advance parser in
      let* parser, op, lbp, rbp = (parse_op parser) in
      if min_bp > lbp then  Ok lhs else 
      let* rhs = parse_expr parser rbp in
        Ok (Expr (lhs, op, rhs))

let parse () = 
  let parser = init in
    parse_expr parser 0.0



let print_factor a = 
  match a with 
    | IntFactor x -> print_endline (string_of_int x)
    | IdentFactor x -> print_endline x
let print_op op = 
  match op with 
    | Multiply -> print_endline "*"
    | Divide -> print_endline "/"
    | Plus -> print_endline "+"
    | Minus -> print_endline "-"

let rec print_expr expr = 
  match expr with
    | Factor a -> print_factor a
    | Expr (lhs, _, _) -> print_expr lhs