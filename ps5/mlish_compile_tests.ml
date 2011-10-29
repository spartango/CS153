open Test_framework

let mk_mlish_compile_test (ml: string) (expected : 'a) (name : string) = 
    let compile_result = 
        let ml_ast = Ml_parse.program Ml_lex.lexer ml in
        let scish_ast = Mlish_compile.compile_exp ml_ast in
        let result = Scish_eval.run scish_ast in
            mk_verbose_expect_test compile_result expected (string_of_int) name

print_string "Hi";;
