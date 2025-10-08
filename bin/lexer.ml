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

exception Lexer_Error_Unknown_Char
exception Lexer_Error_Unexpected_Char
exception Lexer_Error_Missing_Expected_Char of string

let rec lex_identifier line posn str = 
  if posn >= String.length line then (posn, Identifier(str))
  else
    let currChar = String.get line posn in
      match currChar with
        | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '0' -> lex_identifier line (posn + 1) (str ^ String.make  1 currChar)
        | ' ' -> (posn + 1, Identifier (str))
        | _ -> raise Lexer_Error_Unexpected_Char

let rec lex_string line posn str = 
  if posn >= String.length line then raise (Lexer_Error_Missing_Expected_Char "Missing matching double quote for string")
  else
    let currChar = String.get line posn in
      match currChar with
        | '"' -> (posn + 1, String (str))
        | _ -> lex_string line (posn + 1) (str ^ String.make  1 currChar)


(* Lex's a number and returns new position in line, and the token *)
let rec lex_number line posn num = 
  let currChar = String.get line posn in
    match currChar with
      | '0' .. '9' -> if posn + 1 >= String.length line then (posn + 1, Integer (((num * 10) + (Char.code currChar - Char.code '0'))))
                      else lex_number line (posn + 1) ( (num * 10) + (Char.code currChar - Char.code '0'))
      | ' ' -> (posn + 1, Integer(num))
      | _ -> raise Lexer_Error_Unexpected_Char

let is_number = function
  | '0' .. '9' -> true
  | _ -> false

let is_alphabetical = function
 | 'a' .. 'z' | 'A' .. 'Z' -> true
 | _ -> false 



let lex_line input_line = 
  let rec lex_helper posn token_list =
    if posn < (String.length input_line) then 
      let currChar = String.get input_line posn
      in
        match currChar with 
          | '(' -> lex_helper (posn + 1) (OpenRoundBracket :: token_list)
          | ')' -> lex_helper (posn + 1) (CloseRoundBracket :: token_list)
          | ';' -> lex_helper (posn + 1) (Semicolon :: token_list)
          | '"' -> let (new_pos, str) = (lex_string input_line (posn + 1) "") in (lex_helper new_pos (str :: token_list))
          | ' ' -> lex_helper (posn + 1) token_list
          | currChar when is_number currChar -> let (new_pos, num) = (lex_number input_line posn 0) in (lex_helper new_pos (num :: token_list))
          | currChar when is_alphabetical currChar -> let (new_pos, ident) = (lex_identifier input_line posn "") in (lex_helper new_pos (ident :: token_list)) 
          | _ -> raise Lexer_Error_Unknown_Char
    else
      token_list
  in lex_helper 0 []

  let rec lex token_list = 
    try
    let curr_line = (get_next_line()) in
    print_endline curr_line;
      match curr_line with
        | line -> List.append (lex (lex_line line)) token_list
    with e ->
      print_endline (Printexc.to_string e);
      token_list

let token_to_string = function
  | Identifier (x) -> "identifier: " ^ x
  | Integer (x) -> "integer: " ^ string_of_int(x)
  | String (x) -> "string: " ^ x
  | _ -> "To Implement"


let print_tokens () = List.iter (Printf.printf "%s\n") (List.map token_to_string (lex []) )
  


