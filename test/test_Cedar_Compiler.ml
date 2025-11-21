open Alcotest
open Cedar_Compiler
open Lexer
open Parser

let equal_token t1 t2 =
  t1 = t2

let pp_token fmt t =
  Format.fprintf fmt "{ token type = %s}" (token_to_string t)

let token_testable =
  Alcotest.testable pp_token equal_token

let test_lex program expected () =
  let res = lex_text_block program in
    check (list token_testable) "same lists"  expected  res

let test_parser program expected () = 
  let lexed_program = lex_text_block program in
    let res = parse {token_list = lexed_program; token_ptr = 0} in
      match res with 
        | Ok expr -> check string "same string" expected (Parser_printer.get_expr expr)
        | Error e -> fail e


let suite =
  [ "assignment", `Quick, test_lex "a = 5 + b++" [Identifier("a"); Assign; Integer(5); Plus; Identifier ("b"); Increment;];
    "increment", `Quick, test_lex "a--" [Identifier("a"); Decrement];
    "pars_basic_addition", `Quick, test_parser "1 + 2;" "(+ 1 2)";
    "pars_addition_multiplic", `Quick, test_parser "1 + 2 * 3 + 4 * 5;" "(+ (+ 1 (* 2 3)) (* 4 5))";
    "pars_test_all_arith", `Quick, test_parser "1 + 2 * 3 / 4 - (5 + 6) / 7;" "(- (+ 1 (/ (* 2 3) 4)) (/ (+ 5 6) 7))"; 
    "pars_test_brackets", `Quick, test_parser "(1 + 2) * 3;" "(* (+ 1 2) 3)";
    "pars_test", `Quick, test_parser "(1 + 2) * 3 * (4 * 5);" "(* (* (+ 1 2) 3) (* 4 5))" 
  ]

let () =
  Alcotest.run "Lexer" [ "Basics", suite ]