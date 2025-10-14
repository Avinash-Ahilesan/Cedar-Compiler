let file = "./test_code/test_code.cedar"

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


let ic = open_in file

let read_line () =
        try
            input_line ic
        with e ->
            close_in_noerr ic;
            raise e

let get_next_line () = 
        match read_line () with
            | x -> Some x
            | exception End_of_file -> None

exception Lexer_Error_Unknown_Char
exception Lexer_Error_Unexpected_Char
exception Lexer_Error_Missing_Expected_Char of string

let lex_keywords str =
  match str with
    | "if" ->  Some If
    | "else" -> Some Else
    | "fun" -> Some Fun
    | "true" -> Some (Boolean true)
    | "false" -> Some (Boolean false)
    | _ -> None


let rec lex_ident_or_keywords line posn str = 
  if posn >= String.length line then (match (lex_keywords str) with None -> (posn + 1, Identifier (str)) | Some x -> (posn + 1, x))
  else
    let currChar = String.get line posn in
      match currChar with
        | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '0' -> lex_ident_or_keywords line (posn + 1) (str ^ String.make  1 currChar)
        | ' ' ->  (match (lex_keywords str) with None -> (posn + 1, Identifier (str)) | Some x -> (posn + 1, x))
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

let lex_comparator input_line posn = 
  let currChar = String.get input_line posn in 
  let nextChar = if posn + 1 < String.length input_line then Some (String.get input_line (posn + 1)) else None in
  match currChar with 
    | '>' -> (match nextChar with None -> (posn + 1, GreaterThan) | Some next -> (if next = '=' then (posn + 2, GreaterThanEquals) else (posn + 1, GreaterThan)))
    | '<' -> (match nextChar with None -> (posn + 1, LessThan) | Some next -> (if next = '=' then (posn + 2, LessThanEquals) else (posn + 1, LessThan)))
    | _ -> raise Lexer_Error_Unexpected_Char

let lex_line input_line = 
  let rec lex_helper posn token_list =
    if posn < (String.length input_line) then 
      let currChar = String.get input_line posn
      in
      let lex_next new_list = lex_helper (posn + 1) new_list 
      in
        match currChar with 
          | '(' -> lex_next (OpenRoundBracket :: token_list)
          | ')' -> lex_next (CloseRoundBracket :: token_list)
          | '{' -> lex_next (OpenCurlyBracket :: token_list)
          | '}' -> lex_next (CloseCurlyBracket :: token_list)
          | ';' -> lex_next (Semicolon :: token_list)
          | '=' -> lex_next (Assign :: token_list)
          | '+' -> lex_next (Plus :: token_list)
          | '-' -> lex_next (Minus :: token_list)
          | '/' -> lex_next (Divide :: token_list)
          | '*' -> lex_next (Multiply :: token_list)
          | '"' -> let (new_pos, str) = (lex_string input_line (posn + 1) "") in (lex_helper new_pos (str :: token_list))
          | ' ' -> lex_helper (posn + 1) token_list
          | '>' -> let (new_pos, token) = (lex_comparator input_line posn) in (lex_helper new_pos (token :: token_list))
          | '<' -> let (new_pos, token) = (lex_comparator input_line posn) in (lex_helper new_pos (token :: token_list))
          | currChar when is_number currChar -> let (new_pos, num) = (lex_number input_line posn 0) in (lex_helper new_pos (num :: token_list))
          | currChar when is_alphabetical currChar -> let (new_pos, token) = (lex_ident_or_keywords input_line posn "") in (lex_helper new_pos (token :: token_list)) 
          | _ -> raise Lexer_Error_Unknown_Char
    else
      List.rev token_list
  in lex_helper 0 []

  let rec lex token_list = 
    try
    let curr_line = (get_next_line()) in
      match curr_line with
        | Some line -> List.append token_list (lex (lex_line line))
        | None -> token_list
    with e ->
      print_endline (Printexc.to_string e);
      raise e

let token_to_string = function
  | Identifier (x) -> "identifier: " ^ x
  | Integer (x) -> "integer: " ^ string_of_int(x)
  | String (x) -> "string: " ^ x
  | Assign -> "assign: ="
  | If -> "If: if"
  | Boolean (x) -> "bool: " ^ (string_of_bool x)
  | GreaterThan -> ">"
  | _ -> "To Implement"

let token_list = lex []
let token_ptr : int ref = {contents = 0}


let next_token () = 
  if !token_ptr < List.length token_list then 
    let next_token_ptr = !token_ptr in
      token_ptr := !token_ptr + 1;
      List.nth token_list next_token_ptr
  else EOF


