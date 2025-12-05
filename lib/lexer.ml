let file = "test_code/test_code.cedar"

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

let read_line ic () =
      try
          input_line ic
      with e ->
          close_in_noerr ic;
          raise e

let get_next_line ic () = 
        match read_line ic () with
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
        | _ -> (match (lex_keywords str) with None -> (posn, Identifier (str)) | Some x -> (posn, x))

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
      | _ -> (posn, Integer(num))

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
          | ' ' -> lex_helper (posn + 1) token_list (* Ignore whitespace*)
          | '(' -> lex_next (OpenRoundBracket :: token_list)
          | ')' -> lex_next (CloseRoundBracket :: token_list)
          | '{' -> lex_next (OpenCurlyBracket :: token_list)
          | '}' -> lex_next (CloseCurlyBracket :: token_list)
          | '[' -> lex_next (OpenSquareBracket :: token_list)
          | ']' -> lex_next (CloseSquareBracket :: token_list)
          | ';' -> lex_next (Semicolon :: token_list)
          | '=' -> lex_next (Assign :: token_list)
          | '!' -> lex_next (Bang :: token_list)
          | '+' -> if posn + 1 < String.length input_line then 
                      if String.get input_line (posn + 1) = '+' then (lex_helper (posn + 2) (Increment :: token_list))
                      else lex_next (Plus :: token_list)
                  else lex_next (Plus :: token_list)
          | '-' -> if posn + 1 < String.length input_line then 
                      if String.get input_line (posn + 1) = '-' then (lex_helper (posn + 2) (Decrement :: token_list))
                      else lex_next (Minus :: token_list)
                  else lex_next (Minus :: token_list)
          | '/' -> lex_next (Divide :: token_list)
          | '*' -> lex_next (Multiply :: token_list)
          | '"' -> let (new_pos, str) = (lex_string input_line (posn + 1) "") in (lex_helper new_pos (str :: token_list))
          | '>' -> let (new_pos, token) = (lex_comparator input_line posn) in (lex_helper new_pos (token :: token_list))
          | '<' -> let (new_pos, token) = (lex_comparator input_line posn) in (lex_helper new_pos (token :: token_list))
          | currChar when is_number currChar -> let (new_pos, num) = (lex_number input_line posn 0) in (lex_helper new_pos (num :: token_list))
          | currChar when is_alphabetical currChar -> let (new_pos, token) = (lex_ident_or_keywords input_line posn "") in (lex_helper new_pos (token :: token_list)) 
          | _ -> raise Lexer_Error_Unknown_Char
    else
      List.rev token_list
  in lex_helper 0 []

  let lex () = 
    let ic = open_in file in
    let rec lex_helper token_list = 
      try
        let curr_line = (get_next_line ic ()) in
          match curr_line with
            | Some line -> List.append token_list (lex_helper (lex_line line))
            | None -> token_list
      with e ->
        print_endline (Printexc.to_string e);
        raise e 
      in  {token_list = (lex_helper []); token_ptr = 0}

let lex_text_block program = lex_line program

let token_to_string = function
  | Identifier (x) -> "identifier: " ^ x
  | Integer (x) -> "integer: " ^ string_of_int(x)
  | String (x) -> "string: " ^ x
  | Assign -> "assign: ="
  | If -> "If: if"
  | Boolean (x) -> "bool: " ^ (string_of_bool x)
  | GreaterThan -> ">"
  | Plus -> ": +" 
  | Increment -> ": ++"
  | Minus -> ": -"
  | Multiply -> ": *"
  | Divide -> "/"
  | Semicolon -> ";"
  | Equals -> "equals: =="
  | OpenRoundBracket -> "("
  | CloseRoundBracket -> ")"
  | EOF -> "END OF FILE"
  | _ -> "To Implement"


let next_token lexer_state = 
  if  lexer_state.token_ptr < List.length lexer_state.token_list then 
    let next_state = { token_list = lexer_state.token_list; token_ptr = lexer_state.token_ptr + 1} in
    (next_state, List.nth lexer_state.token_list lexer_state.token_ptr)
  else (lexer_state, EOF)


