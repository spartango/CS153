open Test_framework
open Comblexer
open Lcombinators.GenericParsing
open Lcombinators.CharParsing
open Explode

let stub = Test("Implemented", (fun () -> false)  )

let lex_test_inputs = [
    ("foo", [(Id "foo")]);
    ("foo=baz", [(Id "foo"); Assign; (Id "baz")]);
    ("5", [(Int 5)]);
    ("5+9", [(Int 5);Plus;(Int 9)]);
    ("+", [Plus]);
    ("+foo", [Plus; (Id "foo")]);
    ("=", [Assign]);
    ("=foo", [Assign; (Id "foo")]);
    ("-", [Minus]);
    ("-foo+foo", [Minus; (Id "foo"); Plus; (Id "foo")]);
    ("*", [Times]);
    ("*5=foo", [Times; (Int 5); Assign; (Id "foo")]);
    ("/", [Div]);
    ("/feed=foo", [Div; (Id "feed"); Assign; (Id "foo")]);
    ("!=", [Neq]);
    ("!=foo", [Neq; (Id "foo")]);
    (">=", [Gte]);
    (">=55", [Gte; (Int 55)]);
    (">", [Gt]);
    (">foo+5", [Gt;(Id "foo");Plus;(Int 5)]);
    ("<=", [Lte]);
    ("<=foo", [Lte;(Id "foo")]);
    ("==", [Eq]);
    ("==foo", [Eq;(Id "foo")]);
    ("!", [Not]);
    ("!0", [Not;(Int 0)]);
    ("||", [Or]);
    ("||5>=0", [Or;(Int 5);Gte;(Int 0)]);
    ("&&", [And]);
    ("&&6<=34", [And;(Int 6);Lte;(Int 34)]);
    ("(", [LParen]);
    ("(8)", [LParen;(Int 8);RParen]);
    (")", [RParen]);
    (")=(&&", [RParen;Assign;LParen;And]);
    ("{", [LCurly]);
    ("}", [RCurly]);
    ("for{i=4;i<=6;i=i+1}", [For;LCurly;(Id "i");Assign;(Int 4);Seq;(Id "i");Lte;
                             (Int 6);Seq;(Id "i");Assign;(Id "i");Plus;(Int 1);RCurly]);
]

let mk_lex_combinator_test (p: (char, token) parser) (expected_token: token)
        (label: string) =
    let test_map (errors: string) (case: string * token list) : string =
        let(code_string, tkns) = case in
        let cs = explode code_string in
        let head_token = (List.hd tkns) in
            match (p cs) with
                | Cons((tkn,_),_) ->
                      if ((head_token = expected_token) && 
                              (tkn = head_token)) ||
                          ((head_token <> expected_token) && 
                                (tkn <> head_token))
                      then errors
                      else errors ^ "\n\tExpected: " ^ (tkn2str(head_token)) ^ 
                          " Lexed: " ^ (tkn2str(tkn))
                | Nil ->
                      if (head_token == expected_token)
                      then errors ^ "\n\tReturned no token but expected: " ^
                          (tkn2str(head_token))
                      else errors in
    let result = List.fold_left test_map "" lex_test_inputs in
        if (result <> "")
        then Verbose_Test(label, (fun () -> (false, "Lexing error:" ^ result)))
        else Verbose_Test(label, (fun () -> (true, "Lexed expected tokens")))

let test_id_combinator = 
    (mk_lex_combinator_test id_combinator (Id "foo") "Combinator for Id")
let test_int_combinator =
    (mk_lex_combinator_test int_combinator (Int 5) "Combinator for Int");;
let test_plus_combinator =
    (mk_lex_combinator_test plus_combinator (Plus) "Combinator for Plus");;
let test_minus_combinator =
    (mk_lex_combinator_test minus_combinator (Minus) "Minus Combinator");;
let test_times_combinator =
    (mk_lex_combinator_test times_combinator (Times) "Times Combinator");;
let test_div_combinator =
    (mk_lex_combinator_test div_combinator (Div) "Div Combinator");;
let test_neq_combinator =
    (mk_lex_combinator_test neq_combinator (Neq) "Neq Combinator");;
let test_gte_combinator =
    (mk_lex_combinator_test gte_combinator (Gte) "Gte Combinator");;
let test_lte_combinator =
    (mk_lex_combinator_test lte_combinator (Lte) "Lte Combinator");;
let test_eq_combinator =
    (mk_lex_combinator_test eq_combinator (Eq) "Combinator for Eq");;
let test_or_combinator =
    (mk_lex_combinator_test or_combinator (Or) "Or Combinator");;
let test_and_combinator =
    (mk_lex_combinator_test and_combinator (And) "And combinator");;
let test_not_combinator =
    (mk_lex_combinator_test not_combinator (Not) "Not combinator");;
let test_lparen_combinator =
    (mk_lex_combinator_test lparen_combinator (LParen) "LParen combinator");;
let test_rparen_combinator =
    (mk_lex_combinator_test rparen_combinator (RParen) "RParen combinator");;

let test_complete_combinator = 
    let label = "Complete combinator" in
    let test_case (errors: string) (case: string * token list) : string = 
        let (code_string,tkns) = case in
        let cs = explode code_string in
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
        then Verbose_Test(label, (fun () -> (false, result)))
        else Verbose_Test(label, (fun () -> (true, "Lexed expected tokens")));;

let test_tokenizer_snippets =
    let label = "Tokenize Fish snippets" in
    let tkn_list2str (ts: token list) =
        List.fold_left (fun s t -> s ^ " " ^ (tkn2str t)) "" ts in
    let test_case (errors: string) (case: string * token list) : string =
        let (code_string, tkns) = case in
        let cs = explode code_string in
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
              test_eq_combinator;
              test_minus_combinator;
              test_times_combinator;
              test_div_combinator;
              test_neq_combinator;
              test_gte_combinator;
              test_lte_combinator;
              test_or_combinator;
              test_and_combinator;
              test_not_combinator;
              test_lparen_combinator;
              test_rparen_combinator;
              test_complete_combinator;] "Token Combinator Tests";;
run_test_set [test_tokenizer_snippets] "Tokenizer Tests";;




   
    
