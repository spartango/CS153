open Test_framework
open Comblexer
open Lcombinators.GenericParsing
open Lcombinators.CharParsing

let stub = Test("Implemented", (fun () -> false)  )

(* 
let mk_token_eq_test (t: token) =
    mk_verbose_expect_test (fun () -> (token_eq t t)) true string_of_bool 
        ("Equality of " ^ tkn2str(t))

let plus_eq_test = 
    (mk_token_eq_test Plus);;

let id_eq_test =
    (mk_token_eq_test (Id "foo"));;

run_test_set [plus_eq_test;
              id_eq_test] "Token equality tests.";;
*)
let lex_test_inputs = [
    (['f';'o';'o'], [(Id "foo")]);
    (['f';'o';'o';'=';'b';'a';'z'], [(Id "foo"); Eq; (Id "baz")]);

    (['5'], [(Int 5)]);
    (['5';'+';'9'], [(Int 5);Plus;(Int 6)]);
    (['+'], [Plus]);
    (['+';' ';'f';'o';'o'], [Plus; (Id "foo")])
]

let mk_lex_combinator_test (p: (char, token) parser) (expected_token: token)
        (label: string) =
    let test_map (errors: string) (case: char list * token list) : string =
        let(cs, tkns) = case in
        let head_token = (List.hd tkns) in
            match (p cs) with
                | Cons((tkn,_),_) ->
                      if ((head_token = expected_token) && 
                              (tkn = head_token)) ||
                          ((head_token <> expected_token) && 
                                (tkn <> head_token))
                      then errors
                      else errors ^ "\nExpected: " ^ (tkn2str(head_token)) ^ 
                          " Lexed: " ^ (tkn2str(tkn))
                | Nil ->
                      if (head_token == expected_token)
                      then errors ^ "\nReturned no token but expected: " ^
                          (tkn2str(head_token))
                      else errors in
    let result = List.fold_left test_map "" lex_test_inputs in
        if (result <> "")
        then Verbose_Test(result, (fun () -> (false, label)))
        else Verbose_Test("", (fun () -> (true, label)))

let test_id_combinator = 
    (mk_lex_combinator_test id_combinator (Id "foo") "Combinator for Id")
let test_int_combinator =
    (mk_lex_combinator_test int_combinator (Int 5) "Combinator for Int");;
let test_plus_combinator =
    (mk_lex_combinator_test plus_combinator (Plus) "Combinator for Plus");;

let test_complete_combinator = 
    let label = "Test for complete combinator" in
    let test_iterator (errors: string) (case: char list * token list) : string = 
        let (cs,tkns) = case in
        let head_token = List.hd tkns in
            match complete_combinator cs with
                | Cons((tkn,_),_) ->
                      if (tkn = head_token)
                      then errors
                      else errors ^ "\nExpected: " ^ (tkn2str(head_token)) ^ 
                          " Lexed: " ^ (tkn2str(tkn))
                | Nil -> errors ^ "\nReturned no token but expected: " ^
                      (tkn2str(head_token)) in
    let result = List.fold_left test_iterator "" lex_test_inputs in
        if (result <> "")
        then Verbose_Test(result, (fun () -> (false, label)))
        else Verbose_Test("", (fun () -> (true, label)));;

run_test_set [stub] "Test Stub";;
run_test_set [test_id_combinator; 
              test_int_combinator;
              test_plus_combinator;
              test_complete_combinator ] "Token Combinator Tests"




   
    
