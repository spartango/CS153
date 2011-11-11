open Monadic
open Test_framework
open Monad
module S = Scish_ast

exception TypeFailure

let vaL2string (v: Monad.value) =
    let o2s o = match o with 
        | (Var x) -> x
        | (Int i) -> string_of_int i in
    match v with 
        | Op w -> o2s w
        | PrimApp(p, ws) -> (S.primop2string p) ^ "(" ^ (String.concat "," (List.map o2s ws)) ^")"
        | Lambda(x, body) -> Monad.exp2string body

let to_operand (s: string) : Monad.operand =
    try 
        let i = int_of_string s in
            Int(i)
    with 
        | Failure "int_of_string" ->
              Var(s);;

let make_cfold_test in_code (expected: string) (label: string) =
    let (oper, o1, o2) = in_code in
    let test_code (v: Monad.value) = 
        Monad.LetVal("x0",  Monad.Op(Monad.Int(5)), (Monad.LetVal("x1", v, Monad.Return(Monad.Var("x1"))))) in
        mk_expect_test (fun () -> (cfold (test_code (Monad.PrimApp(oper, [to_operand o1; to_operand o2]))))) (test_code (Op (to_operand expected))) label;;

let add_test1 = make_cfold_test (S.Plus, "8", "5") "13" "Add two ints";;
let add_test2 = make_cfold_test (S.Plus, "x0", "0") "x0" "Add value and 0";;
let add_test3 = make_cfold_test (S.Plus, "0", "x0") "x0" "Add 0 and value";;

let minus_test1 = make_cfold_test (S.Minus, "8", "5") "3" "Subtract two ints";;
let minus_test2 = make_cfold_test (S.Minus, "x0", "0") "x0" "Subtract 0 from value";;
let minus_test3 = make_cfold_test (S.Minus, "x0", "x0") "0" "Subtract a value from itself";;

let times_test1 = make_cfold_test (S.Times, "5", "3") "15" "Multiply two ints";;
let times_test2 = make_cfold_test (S.Times, "x0", "0") "0" "Multiply value by zero";;
let times_test3 = make_cfold_test (S.Times, "x0", "1") "x0" "Multiply value by one";;
let times_test4 = make_cfold_test (S.Times, "0", "x0") "0" "Multiply zero by value";;
let times_test5 = make_cfold_test (S.Times, "1", "x0") "x0" "Multiply one by value";;

let div_test1 = make_cfold_test (S.Div, "15", "5") "3" "Divide two ints";;
let div_test2 = make_cfold_test (S.Div, "x0", "1") "x0" "Divide value by one";;
let div_test3 = make_cfold_test (S.Div, "0", "x0") "0" "Divide two ints";;
let div_test4 = make_cfold_test (S.Div, "x0", "x0") "1" "Divide two ints";;

let eq_test1 = make_cfold_test (S.Eq, "15", "15") "1" "Equality of two ints";;
let eq_test2 = make_cfold_test (S.Eq, "x0", "x0") "1" "Equality of two ints";;
let eq_test3 = make_cfold_test (S.Eq, "15", "12") "0" "Inequality of two ints";;

let lt_test1 = make_cfold_test (S.Lt, "15", "15") "0" "Inequality of two ints";;
let lt_test2 = make_cfold_test (S.Lt, "14", "15") "1" "Equality of two ints";;
let lt_test3 = make_cfold_test (S.Lt, "x0", "x0") "0" "Inequality of two ints";;

let mk_inline_thresh_test (e: exp) (thresh: int) (pass: bool) (label: string) =
    mk_verbose_expect_test (fun () -> size_inline_thresh thresh e) pass string_of_bool label;;

(* Size 2 *)
let test_exp1 = (Return(Int(1)))
(* Size 7 *)
let test_exp2 = LetVal("x1", PrimApp(S.Plus, [Int(1); Int(2)]), test_exp1)
(* Size: 5 *)
let test_exp3 = LetVal("x2", Op(Int(2)), test_exp1);;
(* Size: 11 *)
let test_exp4 = LetVal("x3", Lambda("x4", test_exp2), test_exp1);;
(* Size: 5 *)
let test_exp5 = LetCall("x5", Var("x3"), Int(2), test_exp1)
(* Size 14 *)
let test_exp6 = LetVal("x3", Lambda("x4", test_exp2), test_exp5);;
(* Size  13*)
let test_exp7 = LetIf("x6", Int(6), test_exp1, test_exp2, test_exp1);;

let size_test1 = mk_inline_thresh_test test_exp1 3 true "Size of return 1 < 3";;
let size_test2 = mk_inline_thresh_test test_exp1 2 false "Size of return 1 !< 2";;
let size_test3 = mk_inline_thresh_test test_exp2 8 true "Size of LetVal of PrimApp < 8";;
let size_test4 = mk_inline_thresh_test test_exp2 7 false "Size of LetVal of PrimApp !< 7";;
let size_test5 = mk_inline_thresh_test test_exp3 6 true "Size of LetVal of Op < 6";;
let size_test6 = mk_inline_thresh_test test_exp3 5 false "Size of LetVal Op !< 5";;
let size_test7 = mk_inline_thresh_test test_exp4 12 true "Size of LetVal of Lambda < 12";;
let size_test8 = mk_inline_thresh_test test_exp4 11 false "Size of LetVal of Lambda !< 11";;
let size_test9 = mk_inline_thresh_test test_exp6 15 true "Size of LetCall < 15";;
let size_test10 = mk_inline_thresh_test test_exp6 14 false "Size of LetCall !< 14";;
let size_test11 = mk_inline_thresh_test test_exp7 14 true "Size of LetIf < 26";;
let size_test12 = mk_inline_thresh_test test_exp7 13 false "Size of LetIf !< 25";;

run_test_set [add_test1;
             add_test2;
             add_test3;
             minus_test1;
             minus_test2;
             minus_test3;
             times_test1;
             times_test2;
             times_test3;
             times_test4;
             times_test5;
             div_test1;
             div_test2;
             div_test3;
             div_test4;
             eq_test1;
             eq_test2;
             eq_test3;
             lt_test1;
             lt_test2;
             lt_test3] "Constant Folding Tests";;

run_test_set [size_test1;
              size_test2;
              size_test3;
              size_test4;
              size_test5;
              size_test6;
              size_test7;
              size_test8;
              size_test9;
              size_test10;
              size_test11;
              size_test12] "Inline Threshold Size Tests";;
(*
(* Prints the before/after result of one dce pass over the code *)
let show_dce_pass (s: string) : unit =
    let monadized_code = monadize s in
    let _ = print_endline "Initial monadic code:" in
    let _ = print_endline (Monad.exp2string monadized_code) in
    let _ = print_endline "Result of one dead code elimination pass:" in
        print_endline (Monad.exp2string (dce monadized_code));;

show_dce_pass "let x = (fun u -> 5) in let y = 9 in 4";;
*)
