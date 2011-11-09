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

run_test_set [add_test1;
             add_test2;
             add_test3;
             minus_test1;
             minus_test2;] "Constant Folding Tests";;
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
