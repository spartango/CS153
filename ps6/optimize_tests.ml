open Monadic
open Test_framework

exception TypeFailure


let monadize (s: string) : Monad.exp =
    let mlish_code = Ml_parse.program Ml_lex.lexer (Lexing.from_string s) in
        (* Type check *)
    let _ = Mlish_type_check.type_check_exp mlish_code in
        Monadic.tomonadic (Mlish_compile.compile_exp mlish_code)

let make_cfold_test (s: string) (expected: int) (label: string) =
    mk_verbose_expect_test 
        (fun () -> (cfold (monadize ("let y = 5 in " ^ s)))) 
        (Monad.LetVal("x0",  Monad.Op(Monad.Int(5)), (Monad.LetVal("x1", Monad.Op(Monad.Int(expected)), Monad.Return(Monad.Var("x1"))))))
        Monad.exp2string label;;

let add_test1 = make_cfold_test "5 + 3" 8 "Constant folding on add";;
let add_test2 = make_cfold_test "y + 0" 0 "add var and 0";;
run_test_set [add_test1;
             add_test2;] "Constant Folding Tests";;

(* Prints the before/after result of one dce pass over the code *)
let show_dce_pass (s: string) : unit =
    let monadized_code = monadize s in
    let _ = print_endline "Initial monadic code:" in
    let _ = print_endline (Monad.exp2string monadized_code) in
    let _ = print_endline "Result of one dead code elimination pass:" in
        print_endline (Monad.exp2string (dce monadized_code));;

show_dce_pass "let x = (fun u -> 5) in let y = 9 in 4";;
