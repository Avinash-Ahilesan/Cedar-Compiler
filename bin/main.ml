open Cedar_Compiler


let rec print_all_tokens () = 
  match Lexer.next_token() with
    | EOF -> print_endline "EOF"
    | a -> print_endline (Lexer.token_to_string a); print_all_tokens ()

let () = print_all_tokens ()
