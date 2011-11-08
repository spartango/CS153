open Monadic
open Test_framework

exception TypeFailure


let monadize (s: string) : Monad.exp =
    let mlish_code = Ml_parse.program Ml_lex.lexer (Lexing.from_string s) in
        (* Type check *)
    let _ = Mlish_type_check.type_check_exp mlish_code in
        Monadic.tomonadic (Mlish_compile.compile_exp mlish_code)

let make_cfold_test =
    mk_verbose_expect_test 

