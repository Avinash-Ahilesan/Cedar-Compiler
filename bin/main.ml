open Cedar_Compiler


(* let rec print_all_tokens () = 
  match Lexer.next_token() with
    | EOF -> print_endline "EOF"
    | a -> print_endline (Lexer.token_to_string a); print_all_tokens ()

let () = print_all_tokens () *)


let () = match (Parser.parse ()) with
  | Ok e -> Parser_printer.print_expr e 
  | Error s -> print_endline s