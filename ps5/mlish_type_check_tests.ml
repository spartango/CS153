open Test_framework
open Mlish_ast

let nil_string = "(" ^ (Scish_ast.exp2string Mlish_compile.nil_content) ^ " " ^
    (Scish_ast.exp2string Mlish_compile.is_nil) ^")"
let val_unit = string_of_int Mlish_compile.val_unit

(* Type Checks a string of MLish to Scish and evaluates *)
let compile_answer (ml: string) () : (bool * string) =
    let ml_ast = Ml_parse.program Ml_lex.lexer (Lexing.from_string ml) in
    try 
      let t_type = Mlish_type_check.type_check_exp ml_ast in (true, ("Type Checked: "^(type_to_string t_type)))
    with Mlish_type_check.TypeError -> (false, "Type Error Found")

(* Type Checks ML, expecting a string as the return value for cons and #closure *)
let mk_str_compile_test (ml: string) (expected: string) (name: string ) : test =
        Verbose_Test(name, (compile_answer ml))

(* Expects an int for the return value of the evaluation *)
let mk_compile_test (ml: string) (expected: int) (name : string) : test = 
    Verbose_Test(name, (compile_answer ml))

let mk_binop_test (op: string) =
    mk_compile_test ("10" ^ op ^ "5") 

let let_test = mk_compile_test "let x = 5 in x" 5 "Type Check Let"
let fun_test = mk_str_compile_test "(fun x -> 5)" "#closure" "Type Check function"
let app_test = mk_compile_test "(fun x -> 5) 2" 5 "Type Check applied function";;

let compile_int_test = mk_compile_test "1" 1 "Type Check int";;
let compile_unit_test = mk_compile_test "(fun x -> 5) ()" 5 "Type Check unit";;
let bool_true_test = mk_compile_test "true" 1 "Type Check true";;
let bool_false_test = mk_compile_test "false" 0 "Type Check false";;
let plus_test = mk_binop_test "+" 15 "Type Check plus";;
let minus_test1 = mk_binop_test "-" 5 "Type Check minus, part 1";;
let minus_test2 = mk_compile_test " 7 -3" 4 "Type Check minus, part 2";;
let times_test = mk_binop_test "*" 50 "Type Check times";;
let div_test = mk_binop_test "/" 2 "Type Check div";;
let eq_test1 = mk_binop_test "=" 0 "Type Check eq, part 1";;
let eq_test2 = mk_compile_test "10 = 10" 1 "Type Check eq, part 2";;
let lt_test1 = mk_binop_test "<" 0 "Type Check lt, part 1";;
let lt_test2 = mk_compile_test "5<10" 1 "Type Check lt, part 2";;
let pair_test = mk_str_compile_test "(5, 4)" "(5 4)" "Type Check pair";;
let fst_test = mk_compile_test "fst (5, 4)" 5 "Type Check fst";;
let snd_test = mk_compile_test "snd (5, 4)" 4 "Type Check snd";;

let list_test0 = mk_str_compile_test "[]" nil_string "Type Check Nil"
let list_test1 = mk_str_compile_test "[2]" ("((2 " ^ nil_string ^ ") 0)") "Type Check Cons of val and nil";;
let list_test2 = mk_str_compile_test "[2;1]" ("((2 ((1 " ^ nil_string ^ ") 0)) 0)") "Cons of val::val::nil"
let hd_test1 = mk_compile_test "hd [5]" 5 "Type Check hd, part 1";;
let hd_test2 = mk_compile_test "hd [5;4;3]" 5 "Type Check hd, part 2";;
let tl_test1 = mk_str_compile_test "tl [5]" nil_string "Type Check tl, part 1";;
let tl_test2 = mk_str_compile_test "tl [5; 4]" ("((4 " ^ nil_string ^ ") 0)") "Type Check tl, part 2";;
let tl_test3 = mk_str_compile_test "tl [5; 4; 3]" ("((4 ((3 " ^ nil_string ^ ") 0)) 0)") "Type Check tl, part 3";;
let isnil_test1 = mk_compile_test "isnil []" 1 "Isnil test, part 1";;
let isnil_test2 = mk_compile_test "isnil [5]" 0 "Isnil test, part 2";;

let if_true1 = mk_compile_test "if 1 then 5 else 2" 5 "Simplest if-true test";;
let if_true2 = mk_compile_test "if true then 5 else 2" 5 "True if-true test";;
let if_false1 = mk_compile_test "if 0 then 5 else 2" 2 "Simple if-false test";;
let if_false2 = mk_compile_test "if false then 5 else 2" 2 "False if-false test";;

let comp_test1 = mk_compile_test "let x = 5 in let y = 7 in let z = (if y < x then 5 + 2 else 6 / 3) in hd (tl [x;y;z])" 7 "Comprehensive test 1";;
let comp_test2 = mk_compile_test "let test_fun = (fun x -> if x then 4 else 5) in test_fun true" 4 "Comprehensive test 2";;
let comp_test3 = mk_compile_test "let test_fun = (fun x -> (fst x) + (snd x)) in test_fun (10, 3)" 13 "Comprehensive test 3";;
let comp_test4 = mk_compile_test "let test_fun = (fun x -> 5 / 5 + 1) in (test_fun ()) - (test_fun ())" 0 "Comprehensive test 4";;

run_test_set [let_test;
              fun_test;
              app_test; ] "Type Check Exp Tests";;

run_test_set [compile_int_test; 
              compile_unit_test;
              bool_true_test;
              bool_false_test;
              plus_test;
              minus_test1;
              minus_test2;
              times_test;
              div_test;
              eq_test1;
              eq_test2;
              lt_test1;
              lt_test2;
              pair_test;
              fst_test;
              snd_test;
             ] "Type Check PrimApp Tests";;

run_test_set [list_test0;
              list_test1;
              list_test2;
              hd_test1;
              hd_test2;
              tl_test1;
              tl_test2;
              tl_test3;
              isnil_test1;
              isnil_test2;
             ] "Type Check List Tests";;

run_test_set [if_true1;
              if_false1;
              if_true2;
              if_false2;] "Type Check If Exp Tests";;

run_test_set [comp_test1;
              comp_test2;
              comp_test3;
              comp_test4]
    "larger program tests";;
