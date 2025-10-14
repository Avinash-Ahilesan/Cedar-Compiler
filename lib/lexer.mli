type token = 
  | Identifier of string
  (* types *)
  | Boolean of bool
  | Integer of int
  | String of string
  (* symbols *)
  | Assign
  (* comparators *)
  | Equals
  | GreaterThan
  | LessThan
  | GreaterThanEquals
  | LessThanEquals
  (* arithmetic *)
  | Plus
  | Minus
  | Multiply
  | Divide
  (* keywords *)
  | If
  | Else
  | Fun
  (* syntax *)
  | OpenRoundBracket
  | CloseRoundBracket
  | OpenCurlyBracket
  | CloseCurlyBracket
  | Semicolon
  | EOF


val next_token : unit -> token
val token_to_string : token -> string