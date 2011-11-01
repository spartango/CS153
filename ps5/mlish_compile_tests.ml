open Test_framework

let nil_string = "(" ^ (Scish_ast.exp2string Mlish_compile.nil_content) ^ " " ^
    (Scish_ast.exp2string Mlish_compile.is_nil) ^")"
let val_unit = string_of_int Mlish_compile.val_unit

(* Compiles a string of MLish to Scish and evaluates *)
let compile_answer (ml: string) () : string =
    let ml_ast = Ml_parse.program Ml_lex.lexer (Lexing.from_string ml) in
    let scish_ast = Mlish_compile.compile_exp ml_ast in
        Scish_eval.val2string (Scish_eval.run scish_ast)    

(* Compiles ML, expecting a string as the return value for cons and #closure *)
let mk_str_compile_test (ml: string) (expected: string) (name: string ) : test =
        mk_verbose_expect_test (compile_answer ml) expected (fun s -> s) name

(* Expects an int for the return value of the evaluation *)
let mk_compile_test (ml: string) (expected: int) (name : string) : test = 
    mk_verbose_expect_test (compile_answer ml) (string_of_int expected) (fun s -> s) name

let mk_binop_test (op: string) =
    mk_compile_test ("10" ^ op ^ "5") 

let let_test = mk_compile_test "let x = 5 in x" 5 "Compile Let"
let fun_test = mk_str_compile_test "(fun x -> 5)" "#closure" "Compile function"
let app_test = mk_compile_test "(fun x -> 5) 2" 5 "Compile applied function";;

let compile_int_test = mk_compile_test "1" 1 "Compile int";;
let compile_unit_test = mk_compile_test "(fun x -> 5) ()" 5 "Compile unit";;
let bool_true_test = mk_compile_test "true" 1 "Compile true";;
let bool_false_test = mk_compile_test "false" 0 "Compile false";;
let plus_test = mk_binop_test "+" 15 "Compile plus";;
let minus_test1 = mk_binop_test "-" 5 "Compile minus, part 1";;
let minus_test2 = mk_compile_test " 7 -3" 4 "Compile minus, part 2";;
let times_test = mk_binop_test "*" 50 "Compile times";;
let div_test = mk_binop_test "/" 2 "Compile div";;
let eq_test1 = mk_binop_test "=" 0 "Compile eq, part 1";;
let eq_test2 = mk_compile_test "10 = 10" 1 "Compile eq, part 2";;
let lt_test1 = mk_binop_test "<" 0 "Compile lt, part 1";;
let lt_test2 = mk_compile_test "5<10" 1 "Compile lt, part 2";;
let pair_test = mk_str_compile_test "(5, 4)" "(5 4)" "Compile pair";;
let fst_test = mk_compile_test "fst (5, 4)" 5 "Compile fst";;
let snd_test = mk_compile_test "snd (5, 4)" 4 "Compile snd";;

let list_test0 = mk_str_compile_test "[]" nil_string "Compile Nil"
let list_test1 = mk_str_compile_test "[2]" ("((2 " ^ nil_string ^ ") 0)") "Compile Cons of val and nil";;
let list_test2 = mk_str_compile_test "[2;1]" ("((2 ((1 " ^ nil_string ^ ") 0)) 0)") "Cons of val::val::nil"
let hd_test1 = mk_compile_test "hd [5]" 5 "Compile hd, part 1";;
let hd_test2 = mk_compile_test "hd [5;4;3]" 5 "Compile hd, part 2";;
let tl_test1 = mk_str_compile_test "tl [5]" nil_string "Compile tl, part 1";;
let tl_test2 = mk_str_compile_test "tl [5; 4]" ("((4 " ^ nil_string ^ ") 0)") "Compile tl, part 2";;
let tl_test3 = mk_str_compile_test "tl [5; 4; 3]" ("((4 ((3 " ^ nil_string ^ ") 0)) 0)") "Compile tl, part 3";;
let isnil_test1 = mk_compile_test "isnil []" 1 "Isnil test, part 1";;
let isnil_test2 = mk_compile_test "isnil [5]" 0 "Isnil test, part 2";;

let if_true1 = mk_compile_test "if 1 then 5 else 2" 5 "Simplest if-true test";;
let if_true2 = mk_compile_test "if true then 5 else 2" 5 "True if-true test";;
let if_false1 = mk_compile_test "if 0 then 5 else 2" 2 "Simple if-false test";;
let if_false2 = mk_compile_test "if false then 5 else 2" 2 "False if-false test";;

run_test_set [let_test;
              fun_test;
              app_test; ] "Compile Exp Tests";;

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
             ] "Compile PrimApp Tests";;

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
             ] "Compile List Tests";;

run_test_set [if_true1;
              if_false1;
              if_true2;
              if_false2;] "Compile If Exp Tests";;
