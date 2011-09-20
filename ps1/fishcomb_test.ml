open Test_framework
open Comblexer
open Lcombinators.GenericParsing
open Lcombinators.CharParsing

let stub = Test("Implemented", (fun () -> false)  );;
    
run_test_set [stub;] "Test Stub"

let lex_test_inputs = [
    (['f';'o';'o'], [(Var "foo")]);
    (['f';'o';'o';'=';'b';'a';'z'], [(Var "foo"); Eq; (Var "baz")]);
    (['5'], [(Int 5)]) ]

let mk_lex_combinator_test (p: char list -> token) (expected_token: token) =
    let test_map (errors: string) (case: char list * token list) : string =
        let(cs, tkns) = case in
        let head_token = (List.hd tkns) in
        let tkn = p cs in
            if ((head_token == expected_token) && (tkn <> head_token)) ||
                ((head_token <> expected_token) && (tkn == head_token))
            then errors ^ "\nExpected: " ^ (tkn2str(head_token)) ^ 
                " Lexed: " ^ (tkn2str(tkn))
            else errors in
    List.fold_left test_map "" lex_test_inputs
        
    
