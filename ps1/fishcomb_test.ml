open Test_framework
open Comblexer
open Lcombinators.GenericParsing
open Lcombinators.CharParsing

let stub = Test("Implemented", (fun () -> false)  )

let lex_test_inputs = [
    (['f';'o';'o'], [(Id "foo")]);
    (['f';'o';'o';'=';'b';'a';'z'], [(Id "foo"); Assign; (Id "baz")]);
    (['5'], [(Int 5)]);
    (['5';'+';'9'], [(Int 5);Plus;(Int 9)]);
    (['+'], [Plus]);
    (['+';' ';'f';'o';'o'], [Plus; (Id "foo")]);
    (['='], [Assign]);
    (['=';'f';'o';'o'], [Assign; (Id "foo")]);
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
    let test_case (errors: string) (case: char list * token list) : string = 
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
    let result = List.fold_left test_case "" lex_test_inputs in
        if (result <> "")
        then Verbose_Test(result, (fun () -> (false, label)))
        else Verbose_Test("", (fun () -> (true, label)));;

let test_tokenizer_snippets =
    let label = "Tokenize Fish snippets" in
    let tkn_list2str (ts: token list) =
        List.fold_left (fun s t -> s ^ " " ^ (tkn2str t)) "" ts in
    let test_case (errors: string) (case: char list * token list) : string =
        let (cs, tkns) = case in
        let lex = tokenize cs in
            if lex = tkns
            then errors
            else errors ^ "\nExpected:" ^ (tkn_list2str tkns) ^ " Lexed:" ^
                (tkn_list2str lex) in
        Verbose_Test(label, 
                     (fun () -> 
                          let result =
                              List.fold_left test_case "" lex_test_inputs in
                              ((result = ""), (
                               if result = ""
                               then "Lexed expected tokens"
                               else "Lexed tokens did not match expected:" ^ 
                                   result ^ "\n"))));;

run_test_set [test_id_combinator; 
              test_int_combinator;
              test_plus_combinator;
              test_complete_combinator ] "Token Combinator Tests";;
run_test_set [test_tokenizer_snippets] "Tokenizer Tests";;




   
    
