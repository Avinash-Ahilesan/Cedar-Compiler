open Alcotest
open Cedar_Compiler
open Lexer


let equal_token t1 t2 =
  t1 = t2

let pp_token fmt t =
  Format.fprintf fmt "{ token type = %s}" (token_to_string t)

let token_testable =
  Alcotest.testable pp_token equal_token

let test_lex program () =
  let res = lex_text_block program in
    let expected = [Identifier("a"); Assign; Integer(5)] in
    check (list token_testable) "same lists"  expected  res


let suite =
  [ "can greet Tom", `Quick, test_lex "a = 5"
  ]

let () =
  Alcotest.run "Lexer" [ "Basics", suite ]