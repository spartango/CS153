open Test_framework
open Fishyacc
open Eval

(* Assumes all test files in test *)
let mk_file_test (file: string) (expected: int) =
    mk_verbose_expect_test (fun () -> (eval (parse_code ("/test" ^ file)))) expected string_of_int
        ("Testing program in " ^ file)

let test_silly =
    (mk_file_test "silly.fish" 5);;

let stub = Test("Implemented", (fun () -> false)  );;
    
run_test_set [test_silly] "Yacc/Lex Tests"
