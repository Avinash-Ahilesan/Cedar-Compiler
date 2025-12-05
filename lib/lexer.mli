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
  | Increment
  | Decrement
  (* keywords *)
  | If
  | Else
  | Fun
  (* syntax *)
  | OpenRoundBracket
  | CloseRoundBracket
  | OpenCurlyBracket
  | CloseCurlyBracket
  | OpenSquareBracket
  | CloseSquareBracket
  | Semicolon
  | Bang
  | EOF

type lexer = {
  token_list: token list;
  token_ptr: int;
}

val next_token : lexer -> lexer * token
val token_to_string : token -> string

val lex_text_block: string -> token list

val lex: unit -> lexer