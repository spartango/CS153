open Test_framework

let compile_answer (ml: string) () : string =
    let ml_ast = Ml_parse.program Ml_lex.lexer (Lexing.from_string ml) in
    let scish_ast = Mlish_compile.compile_exp ml_ast in
        Scish_eval.val2string (Scish_eval.run scish_ast)    

let mk_compile_test (ml: string) (expected: int) (name : string) : test = 
    mk_verbose_expect_test (compile_answer ml) (string_of_int expected) (fun s -> s) name

let let_test = mk_compile_test "let x = 5 in x" 5 "Compile Let"
let fun_test = mk_verbose_expect_test (compile_answer "(fun x -> 5)") "#closure" (fun s -> s) "Compile function"
let app_test = mk_compile_test "(fun x -> 5) 2" 5 "Compile applied function";;
let if_true_test = mk_compile_test "if 1 then 5 else 2" 5 "Simple if-true test"
let if_false_test = mk_compile_test "if 0 then 5 else 2" 2 "Simple if-false test"

let compile_int_test = mk_compile_test "1" 1 "Compile int";;
let compile_unit_test = mk_compile_test "(fun x -> 5) ()" 5 "Compile unit";;

run_test_set [let_test;
              fun_test;
              app_test;
              if_true_test;
              if_false_test; ] "Compile Exp Tests";;

run_test_set [compile_int_test; 
              compile_unit_test] "Compile PrimApp Tests";;
