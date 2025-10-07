let file = "./test_code/test_code.cedar"

type token = 
  | Identifier of string
  (* types *)
  | Boolean of bool
  | Integer of int
  | String of string
  (* symbols *)
  | Assign
  (* conditionals *)
  | Equals
  | GreaterThan
  | LessThan
  | GreaterThanEquals
  | LessThanEquals
  (* syntax *)
  | OpenRoundBracket
  | CloseRoundBracket
  | OpenCurlyBracket
  | CloseCurlyBracket
  | Semicolon


let ic = open_in file

let read_line () =
        try
            input_line ic
        with e ->
            close_in_noerr ic;
            raise e

let get_next_line () = 
    let next_line = read_line () in
        match next_line with
            | x -> x
            | exception End_of_file -> raise End_of_file
  
let rec lex_string line posn str = 
  let currChar = String.get line posn in
    match currChar with
      | '"' -> Identifier (str)
      | _ -> lex_string line (posn + 1) (str ^ String.make  1 currChar)


exception End_of_line
exception Lexer_Error_Unknown_Char
exception Lexer_Error_Unexpected_Char

(* Lex's a number and returns new position in line, and the token *)
let rec lex_number line posn num = 
  let currChar = String.get line posn in
    match currChar with
      | '0' .. '9' -> if posn + 1 >= String.length line then ((num * 10) + (Char.code currChar - Char.code '0')) 
                      else lex_number line (posn + 1) ( (num * 10) + (Char.code currChar - Char.code '0'))
      | _ -> num

let is_number = function
  | '0' .. '9' -> true
  | _ -> false



let lex_line input_line = 
  let rec lex_helper posn token_list =
    let currChar = if posn < String.length input_line then (String.get input_line posn) else raise End_of_line
    in
      match currChar with 
        | '(' -> lex_helper (posn + 1) (OpenRoundBracket :: token_list)
        | ')' -> lex_helper (posn + 1) (CloseRoundBracket :: token_list)
        | ';' -> lex_helper (posn + 1) (Semicolon :: token_list)
        | '"' -> (lex_string input_line (posn + 1) "") :: token_list
        | ' ' -> lex_helper (posn + 1) token_list
        | currChar when is_number currChar -> Integer (lex_number input_line posn 0) :: token_list
        | _ -> raise Lexer_Error_Unknown_Char
  in lex_helper 0 []

  let rec lex token_list = 
    try
    let curr_line = (get_next_line()) in
      match curr_line with
        | line -> List.append ((lex (lex_line line))) token_list
    with _ ->
      token_list

let token_to_string = function
  | Identifier (x) -> "identifier: " ^ x
  | Integer (x) -> "integer: " ^ string_of_int(x)
  | String (x) -> "string: " ^ x
  | _ -> "To Implement"


let print_tokens () = List.iter (Printf.printf "%s\n") (List.map token_to_string (lex []) )
  


