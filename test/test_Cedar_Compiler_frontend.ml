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

let test_parser_success program expected () = 
  let lexed_program = lex_text_block program in
    let stmt_list = parse {token_list = lexed_program; token_ptr = 0} in
      match stmt_list with 
        | Ok stmts -> check string "same string" expected (Parser_printer.get_statement_list stmts)
        | Error e -> fail e

let test_parser_fail program expected_msg () = 
    let lexed_program = lex_text_block program in
    let res = parse {token_list = lexed_program; token_ptr = 0} in
      match res with 
        | Ok _ -> fail "didn't expect success"
        | Error e -> check string "same string" expected_msg e


let lexer_basic = [
    "assignment", `Quick, test_lex "a = 5 + b++" [Identifier("a"); Assign; Integer(5); Plus; Identifier ("b"); Increment;];
    "increment", `Quick, test_lex "a--" [Identifier("a"); Decrement]; 
]

let parser_single_line_exprs =
  [ "pars_basic_addition", `Quick, test_parser_success "(1 + 2);" "(+ 1 2)";
    "pars_addition_multiplic", `Quick, test_parser_success "1 + 2 * 3 + 4 * 5;" "(+ (+ 1 (* 2 3)) (* 4 5))";
    "pars_test_all_arith", `Quick, test_parser_success "1 + 2 * 3 / 4 - (5 + 6) / 7;" "(- (+ 1 (/ (* 2 3) 4)) (/ (+ 5 6) 7))"; 
    "pars_test_brackets", `Quick, test_parser_success "(1 + 2) * 3;" "(* (+ 1 2) 3)";
    "pars_test_missing_brackets", `Quick, test_parser_fail "(1 + 2 * 3;" "Missing closing bracket, received: END OF FILE";
    "pars_test", `Quick, test_parser_success "(1 + 2) * 3 * (4 * 5);" "(* (* (+ 1 2) 3) (* 4 5))";
    "pars_test_prefix_increment", `Quick, test_parser_success "++a * 2 * 3;" "(* (* (++ a) 2) 3)";
    "pars_test_postfix_increment", `Quick, test_parser_success "a++ * 2 * 3;" "(* (* (a ++) 2) 3)";
    "pars_test_prefix2", `Quick, test_parser_success "1 + ++a + 2;" "(+ (+ 1 (++ a)) 2)";
    "pars_test_prefix2", `Quick, test_parser_success "++a++;" "((++ a) ++)";
    "pars_test_arr_index", `Quick, test_parser_success "a[55];" "([] a 55)";
    "pars_test_func_call", `Quick, test_parser_success "a(x, 5, y, 10) + 20;" "(+ (() a x 5 y 10) 20)";
  ]

let parser_multi_line_statements = [
  "two_exprs", `Quick, test_parser_success "(1 + 2); 2 + 3;" "(+ 1 2)\n(+ 2 3)";
  "variable_assign", `Quick, test_parser_success "a = 5 + 6;" "(= a (+ 5 6))" ;
  "two_statements_assign_expr", `Quick, test_parser_success "b = 4+5; 2*9+3;" "(= b (+ 4 5))\n(+ (* 2 9) 3)";
  "multi_assorted_assign_and_arithmetic", `Quick, test_parser_success "2*9+4; diablo = 34; diablo + 3; x = 55; x - 2;" "(+ (* 2 9) 4)\n(= diablo 34)\n(+ diablo 3)\n(= x 55)\n(- x 2)";
  "if_statement_no_else", `Quick, test_parser_success "if (1 + 2) {a = 5; d = 2 + 5; }" "(if (+ 1 2) ((= a 5)\n(= d (+ 2 5))))";
  "if_statement_with_else", `Quick, test_parser_success "if (1 + 2) {a = 5;} else {d - 3; a = a + 5;}" "(if (+ 1 2) ((= a 5)) ((- d 3)\n(= a (+ a 5))))";
  "nested_if_statement", `Quick, test_parser_success "if (x - 5) {if (2 - 1) {b = 6;}}" "(if (- x 5) ((if (- 2 1) ((= b 6)))))"
]


let () =
  Alcotest.run "Compiler Lexer" [ "Lexer Basics", lexer_basic;
                                  "Parser Single Line Exprs", parser_single_line_exprs;
                                  "Parser Multi Line Statements", parser_multi_line_statements]
