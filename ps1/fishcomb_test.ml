open Test_framework
open Lcombinators.GenericParsing
open Comblexer
open Ast
open Combparser 
open Lcombinators.CharParsing
open Explode
open Eval
    
(* Tests for the Parser *) 

let test_token_equal = 
    (mk_expect_test 
        (fun () ->
            let input_tokens = [ (Comblexer.Return, 0);] in
             ((token_equal Comblexer.Return) input_tokens)
        ) 
        (singleton ((Comblexer.Return, 0), []))
        ("Token Equality Test") 
    )
;;

let test_int_alone = 
   Test(     
        "Standalone Int Test",
        (fun () ->
            let input_tokens = [ (Comblexer.Int(1), 0);] in
            let parsed = (parse_int_init input_tokens) in
            match parsed with 
            | Cons(((Ast.Int(1), 0), []), _) -> true
            | _ -> false
        )) 

;;

let test_var_alone = 
   Test(     
        "Standalone Var Test",
        (fun () ->
            let input_tokens = [ (Comblexer.Id("x"), 0);] in
            let parsed = (parse_var_init input_tokens) in
            match parsed with 
            | Cons(((Ast.Var("x"), 0), []), _) -> true
            | _ -> false
        )) 

;;

let test_paren_alone = 
   Test(     
        "Parens Simple Test",
        (fun () ->
            let input_tokens = 
                [ (LParen, 0); (Comblexer.Id("x"), 0); (RParen, 0);] in
            let parsed = (parse_paren_expr input_tokens) in
            match parsed with 
            | Cons(((Ast.Var("x"), 0), []), _) -> true
            | _ -> false
        )) 

;;

let test_negative_int = 
   Test(     
        "Negative Int Test",
        (fun () ->
            let input_tokens = 
                [   (Comblexer.Minus, 0);
                    (Comblexer.Int(1), 0);
                ] in
            let parsed = (parse_int_init input_tokens) in
            match parsed with 
            | Cons(((Ast.Int(-1), 0), []), _) -> true
            | _ -> false
        )) 

;;


let test_int_simple_op = 
   Test(     
        "Int 1 + 1 Test",
        (fun () ->
            let input_tokens = 
                [ (Comblexer.Int(1), 0); 
                  (Comblexer.Plus, 0);
                  (Comblexer.Int(1), 0);] in
            let parsed = (parse_int_init input_tokens) in
            match parsed with 
            | Cons(
                    (
                        ((Ast.Binop(
                            (Ast.Int(1), 0),
                            Ast.Plus,
                            (Ast.Int(1), 0)))
                        , 0), 
                     [])
                  , _ ) -> true

            | _ -> false
        )
   )

;;
    
let test_var_simple_op = 
   Test(     
        "Var x + 1 Test",
        (fun () ->
            let input_tokens = 
                [ (Comblexer.Id("x"), 0); 
                  (Comblexer.Plus, 0);
                  (Comblexer.Int(1), 0);] in
            let parsed = (parse_var_init input_tokens) in
            match parsed with 
            | Cons(
                    (
                        ((Ast.Binop(
                            (Ast.Var("x"), 0),
                            Ast.Plus,
                            (Ast.Int(1), 0)))
                        , 0), 
                     [])
                  , _ ) -> true

            | _ -> false
        ))
;;


let test_var_var_simple_op = 
   Test(     
        "Var x + x Test",
        (fun () ->
            let input_tokens = 
                [ (Comblexer.Id("x"), 0); 
                  (Comblexer.Plus, 0);
                  (Comblexer.Id("x"), 0);] in
            let parsed = (parse_var_init input_tokens) in
            match parsed with 
            | Cons(
                    (
                        ((Ast.Binop(
                            (Ast.Var("x"), 0),
                            Ast.Plus,
                            (Ast.Var("x"), 0)))
                        , 0), 
                     [])
                  , _ ) -> true

            | _ -> false
        ))
;;


let test_var_neg_simple_op = 
   Test(     
        "Var x - 1 Test",
        (fun () ->
            let input_tokens = 
                [ (Comblexer.Id("x"), 0); 
                  (Comblexer.Minus, 0);
                  (Comblexer.Int(1), 0);] in
            let parsed = (parse_var_init input_tokens) in
            match parsed with 
            | Cons(
                    (
                        ((Ast.Binop(
                            (Ast.Var("x"), 0),
                            Ast.Minus,
                            (Ast.Int(1), 0)))
                        , 0), 
                     [])
                  , _ ) -> true

            | _ -> false
        ))
;;


let test_var_and_op = 
   Test(     
        "Var x && 1 Test",
        (fun () ->
            let input_tokens = 
                [ (Comblexer.Id("x"), 0); 
                  (Comblexer.And, 0);
                  (Comblexer.Int(1), 0);] in
            let parsed = (parse_var_init input_tokens) in
            match parsed with 
            | Cons(
                    (
                        ((Ast.And(
                            (Ast.Var("x"), 0),
                            (Ast.Int(1), 0)))
                        , 0), 
                     [])
                  , _ ) -> true

            | _ -> false
        ))
;;

run_test_set [ test_token_equal;
               test_int_alone;
               test_negative_int;
               test_var_alone;
               test_paren_alone;
               test_int_simple_op;
               test_var_simple_op;
               test_var_var_simple_op;
               test_var_neg_simple_op;
               test_var_and_op;
             ]
             "Parser Building Blocks"
;;


let test_simple_var_expr = 
   Test(     
        "Expr x + 1 Test",
        (fun () ->
            let input_tokens = 
                [ (Comblexer.Id("x"), 0); 
                  (Comblexer.Plus, 0);
                  (Comblexer.Int(1), 0);] in
            let parsed = (parse_expression input_tokens) in
            match parsed with 
            | Cons(
                    (
                        ((Ast.Binop(
                            (Ast.Var("x"), 0),
                            Ast.Plus,
                            (Ast.Int(1), 0)))
                        , 0), 
                     [])
                  , _ ) -> true

            | _ -> false
        ))
;;

let test_simple_int_expr = 
   Test(     
        "Expr 1 + x Test",
        (fun () ->
            let input_tokens = 
                [ (Comblexer.Int(1), 0); 
                  (Comblexer.Plus, 0);
                  (Comblexer.Id("x"), 0);] in
            let parsed = (parse_expression input_tokens) in
            match parsed with 
            | Cons(
                    (
                        ((Ast.Binop(
                            (Ast.Int(1), 0),
                            Ast.Plus,
                            (Ast.Var("x"), 0)))
                        , 0), 
                     [])
                  , _ ) -> true

            | _ -> false
        ))
;;

let test_paren_var_expr = 
   Test(     
        "Expr (x + 1) Test",
        (fun () ->
            let input_tokens = 
                [ (Comblexer.LParen, 0);
                  (Comblexer.Id("x"), 0); 
                  (Comblexer.Plus, 0);
                  (Comblexer.Int(1), 0);
                  (Comblexer.RParen, 0)
                ] in
            let parsed = (parse_expression input_tokens) in
            match parsed with 
            | Cons(
                    (
                        ((Ast.Binop(
                            (Ast.Var("x"), 0),
                            Ast.Plus,
                            (Ast.Int(1), 0)))
                        , 0), 
                     [])
                  , _ ) -> true

            | _ -> false
        ))
;;

let test_rgroup_expr = 
   Test(     
        "Expr 2 + (x + 1) Test",
        (fun () ->
            let input_tokens = 
                [ (Comblexer.Int(2), 0);
                  (Comblexer.Plus, 0);
                  (Comblexer.LParen, 0);
                  (Comblexer.Id("x"), 0); 
                  (Comblexer.Plus, 0);
                  (Comblexer.Int(1), 0);
                  (Comblexer.RParen, 0)
                ] in
            let parsed = (parse_expression input_tokens) in
            match parsed with 
            | Cons(
                    (
                        ((Ast.Binop(
                            (Ast.Int(2), 0),
                            Ast.Plus, 
                        ((Ast.Binop(
                            (Ast.Var("x"), 0),
                            Ast.Plus,
                            (Ast.Int(1), 0))), 0)
                        )), 0), 
                     [])
                  , _ ) -> true

            | _ -> false
        ))
;;

let test_not_var_expr = 
   Test(     
        "Expr !(x + 1) Test",
        (fun () ->
            let input_tokens = 
                [ (Comblexer.Not, 0);
                  (Comblexer.LParen, 0);
                  (Comblexer.Id("x"), 0); 
                  (Comblexer.Plus, 0);
                  (Comblexer.Int(1), 0);
                  (Comblexer.RParen, 0)
                ] in
            let parsed = (parse_expression input_tokens) in
            match parsed with 
            | Cons(
                    ((Ast.Not(
                        ((Ast.Binop(
                            (Ast.Var("x"), 0),
                            Ast.Plus,
                            (Ast.Int(1), 0)))
                        , 0)), 0), 
                     [])
                  , _ ) -> true

            | _ -> false
        ))
;;

let test_lgroup_expr = 
   Test(     
        "Expr (x + 1) + 2 Test",
        (fun () ->
            let input_tokens = 
                [ 
                  (Comblexer.LParen, 0);
                  (Comblexer.Id("x"), 0); 
                  (Comblexer.Plus, 0);
                  (Comblexer.Int(1), 0);
                  (Comblexer.RParen, 0);
                  (Comblexer.Plus, 0);
                  (Comblexer.Int(2), 0);
                ] in
            let parsed = (parse_expression input_tokens) in
            match parsed with 
            | Cons(
                    (
                        ((Ast.Binop(
                        ((Ast.Binop(
                            (Ast.Var("x"), 0),
                            Ast.Plus,
                            (Ast.Int(1), 0))), 0),
                            Ast.Plus, 
                            (Ast.Int(2), 0)
                        )), 0), 
                     [])
                  , _ ) -> true

            | _ -> false
        ))
;;

run_test_set [ test_simple_var_expr;
               test_simple_int_expr;
               test_paren_var_expr;
               test_rgroup_expr;
               test_lgroup_expr;
               test_not_var_expr;
             ]
             "Expression Parsing"
;;

(* Statement Parsing Tests *)

let test_s_expr = 
    Test(
         "Statement x + 1 Test",
         (fun () -> 
             let input_tokens =
                 [
                     (Comblexer.Id("x"), 0);
                     (Comblexer.Plus, 0);
                     (Comblexer.Int(1), 0); 
                 ]
             in 
             let parsed = (parse_statement input_tokens) in
             match parsed with 
             | Cons(
                     (
                         (Ast.Exp(
                         ((Ast.Binop(
                             (Ast.Var("x"), 0),
                             Ast.Plus,
                             (Ast.Int(1), 0)))
                         , 0)), 0),  
                      [])
                   , _ ) -> true

             | _ -> false
         ))
;;

let test_return = 
    Test(
         "Statement return x + 1 Test",
         (fun () -> 
             let input_tokens =
                 [
                     (Comblexer.Return, 0);
                     (Comblexer.Id("x"), 0);
                     (Comblexer.Plus, 0);
                     (Comblexer.Int(1), 0); 
                 ]
             in 
             let parsed = (parse_statement input_tokens) in
             match parsed with 
             | Cons(
                     (
                         (Ast.Return(
                         ((Ast.Binop(
                             (Ast.Var("x"), 0),
                             Ast.Plus,
                             (Ast.Int(1), 0)))
                         , 0)), 0),  
                      [])
                   , _ ) -> true

             | _ -> false
         ))
;;

let test_seq = 
    Test(
         "Statement { x + 1; y + 1; } Test",
         (fun () -> 
             let input_tokens =
                 [
                     (Comblexer.LCurly, 0);
                     (Comblexer.Id("x"), 0);
                     (Comblexer.Plus, 0);
                     (Comblexer.Int(1), 0);
                     (Comblexer.Semi, 0);
                     (Comblexer.Id("y"), 0);
                     (Comblexer.Plus, 0);
                     (Comblexer.Int(1), 0);
                     (Comblexer.RCurly, 0);
                 ]
             in 
             let parsed = (parse_statement input_tokens) in
             match parsed with 
             | Cons(
                     (
                         (Ast.Seq(
                         (Ast.Seq( _ , 
                         (Ast.Exp(((Ast.Binop(
                             (Ast.Var("x"), 0),
                             Ast.Plus,
                             (Ast.Int(1), 0)))
                         , 0)), 0)), 0), 
                         (Ast.Exp(((Ast.Binop(
                             (Ast.Var("y"), 0),
                             Ast.Plus,
                             (Ast.Int(1), 0)))
                         , 0)), 0)
                         )
                         , 0)
                         , 
                      [])
                   , _ ) -> true

             | _ -> false
         ))
;;

let test_while = 
    Test( 
        "Simple While loop Test",
        (fun () -> 
            let input_tokens = 
                [
                    (Comblexer.While, 0);
                    (Comblexer.LParen, 0); 
                    (Comblexer.Int(0), 0);
                    (Comblexer.RParen, 0);
                    (Comblexer.Id("x"), 0);
                    (Comblexer.Semi, 0);
                ]
             in 
             let parsed = parse_statement input_tokens in
             match parsed with 
             | Cons( 
                     ( 
                         (Ast.While(
                             (Ast.Int(0), 0), 
                             (Ast.Exp(
                                 (Ast.Var("x"), 0)), 0)
                         ), 0), []), _) -> true
             | _ -> false
        ))
;;

let test_if = 
    Test( 
        "Simple If statement Test",
        (fun () -> 
            let input_tokens = 
                [
                    (Comblexer.If, 0);
                    (Comblexer.LParen, 0); 
                    (Comblexer.Int(0), 0);
                    (Comblexer.RParen, 0);
                    (Comblexer.Id("x"), 0);
                    (Comblexer.Semi, 0);
                ]
             in 
             let parsed = parse_statement input_tokens in
             match parsed with 
             | Cons( 
                     ( 
                         (Ast.If(
                             (Ast.Int(0), 0), 
                             (Ast.Exp(
                                 (Ast.Var("x"), 0)), 0),
                             _
                         ), 0), []), _) -> true
             | _ -> false
        ))
;;

let test_if_else = 
    Test( 
        "If else statement Test",
        (fun () -> 
            let input_tokens = 
                [
                    (Comblexer.If, 0);
                    (Comblexer.LParen, 0); 
                    (Comblexer.Int(0), 0);
                    (Comblexer.RParen, 0);
                    (Comblexer.Id("x"), 0);
                    (Comblexer.Semi, 0);
                    (Comblexer.Else, 0);
                    (Comblexer.Id("y"), 0);
                    (Comblexer.Semi, 0);
                ]
             in 
             let parsed = parse_statement input_tokens in
             match parsed with 
             | Cons( 
                     ( 
                         (Ast.If(
                             (Ast.Int(0), 0), 
                             (Ast.Exp(
                                 (Ast.Var("x"), 0)), 0),
                             (Ast.Exp(
                                 (Ast.Var("y"), 0)), 0)
                         ), 0), []), _) -> true
             | _ -> false
        ))
;;

let test_for = 
    Test( 
         "For loop Test", 
         (fun () -> 
             let input_tokens = 
                 [
                     (Comblexer.For, 0);
                     (Comblexer.LParen, 0);
                     (Comblexer.Id("i"), 0);
                     (Comblexer.Assign, 0);
                     (Comblexer.Int(0), 0);
                     (Comblexer.Semi, 0);
                     (Comblexer.Id("i"), 0);
                     (Comblexer.Eq, 0);
                     (Comblexer.Int(1), 0);
                     (Comblexer.Semi, 0);
                     (Comblexer.Id("i"), 0);
                     (Comblexer.Assign, 0);
                     (Comblexer.Id("i"), 0);
                     (Comblexer.Plus, 0);
                     (Comblexer.Int(1), 0);
                     (Comblexer.RParen, 0);
                     (Comblexer.Id("i"), 0);
                     (Comblexer.Semi, 0)
                 ]
             in
             let parsed = parse_statement input_tokens in
             match parsed with 
             | Cons( 
                        (
                           (Ast.For(
                               (Ast.Assign(
                                   "i", 
                                   (Ast.Int(0), 0)), 0),
                               (Ast.Binop(
                                   (Ast.Var("i"), 0), 
                                   Ast.Eq, 
                                   (Ast.Int(1), 0)), 0),
                               (Ast.Assign(
                                   "i", 
                                   (Ast.Binop(
                                       (Ast.Var("i"), 0),
                                       Ast.Plus,
                                       (Ast.Int(1), 0)), 0)), 0), 
                               (Ast.Exp(
                                   (Ast.Var("i"), 0)), 0)), 0), 
                      []), _) -> true;
             | _ -> false
         ))
;;

run_test_set [ test_s_expr;
               test_return;
               test_seq;
               test_while;
               test_if;
               test_if_else;
               test_for;
             ]
             "Statement Parsing"
;;

let mk_parse_test (file: string) =
    let label = "Lexing & Parsing " ^ file in
        Verbose_Test(label, (fun()->
                                 try 
                                     let ic  = (open_in file) in
                                     let rec read_file accum = 
                                        try 
                                            (read_file accum ^ "\n" ^ (input_line ic))
                                        with End_of_file -> accum
                                     in 
                                     let file_contents = (read_file "") in
                                     let rslt = eval (parse (tokenize ( explode file_contents ))) in
                                         (true, "Parsed with answer " ^ string_of_int rslt)
                                 with  LexError        -> 
                                                (false, "Lexer Failed")
                                       | InvalidSyntax -> 
                                                 (false, "Parser Failed")))

let file_list = 
[
             "test/01cexpr_01add.fish";
             "test/01cexpr_02sub.fish";
             "test/01cexpr_03mul.fish";
             "test/01cexpr_04div.fish";
             "test/01cexpr_05eq.fish";
             "test/01cexpr_06neq.fish";
             "test/01cexpr_07gt.fish";
             "test/01cexpr_08gte.fish";
             "test/01cexpr_09lt.fish";
             "test/01cexpr_10lte.fish";
             "test/01cexpr_11uminus.fish";
             "test/01cexpr_12not.fish";
             "test/01cexpr_13and.fish";
             "test/01cexpr_14or.fish";
             "test/01cexpr_15bigcon.fish";
             "test/01cexpr_16bigcon.fish";
             "test/02vexpr_01add.fish";
             "test/02vexpr_02sub.fish";
             "test/02vexpr_03mul.fish";
             "test/02vexpr_04div.fish";
             "test/02vexpr_05eq.fish";
             "test/02vexpr_06neq.fish";
             "test/02vexpr_07gt.fish";
             "test/02vexpr_08gte.fish";
             "test/02vexpr_09lt.fish";
             "test/02vexpr_10lte.fish";
             "test/02vexpr_11uminus.fish";
             "test/02vexpr_12not.fish";
             "test/02vexpr_13and.fish";
             "test/02vexpr_14or.fish";
             "test/02vexpr_15assignval.fish";
             "test/03stmt_01if.fish";
             "test/03stmt_02if.fish";
             "test/03stmt_03while.fish";
             "test/03stmt_04for.fish";
             "test/03stmt_05if.fish";
             "test/03stmt_06for.fish";
             "test/04opt_01cfoldif.fish";
             "test/04opt_02cfoldif.fish";
             "test/09all_01adder.fish";
             "test/09all_02fibo.fish";
]
;;

run_test_set (List.map mk_parse_test file_list) "Parse Test Files";;

(*
let lex_test_inputs = [
    ("foo", [(Id "foo")]);
    ("foo=baz", [(Id "foo"); Comblexer.Assign; (Id "baz")]);
    ("5", [(Comblexer.Int 5)]);
    ("5+9", [(Comblexer.Int 5);Plus;(Int 9)]);
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
    ("for{i=4;i<=6;i=i+1}", [For;LCurly;(Id "i");Assign;(Int 4);Semi;(Id "i");Lte;
                             (Int 6);Semi;(Id "i");Assign;(Id "i");Plus;(Int 1);
                             RCurly]);
    ("if(i==5){k=2;}else{k=1;}", [If;LParen;(Id "i");Eq;(Int 5);RParen;LCurly;
                                  (Id "k");Assign;(Int 2);Semi;RCurly;Else;LCurly;
                                  (Id "k");Assign;(Int 1);Semi;RCurly]);
    ("while(i>=0){k=k-1;i=i-2;}", [While;LParen;(Id "i");Gte;(Int 0);RParen;LCurly;
                                   (Id "k");Assign;(Id "k");Minus;(Int 1);Semi;
                                   (Id "i");Assign;(Id "i");Minus;(Int 2);Semi;
                                   RCurly]);
    ("forever", [(Id "forever")]);
    ("if_i_am", [(Id "if_i_am")]);
    ("elsey", [(Id "elsey")]);
    ("whiley", [(Id "whiley")]);
    ("+/*I am a comment*/5", [Plus;(Int 5)]);
]

let mk_lex_combinator_test (p: (char, rtoken) parser) (expected_token: rtoken)
        (label: string) =
    let tkn_match (t1: rtoken) (t2: rtoken) : bool  =
        match (t1, t2) with
            | ((Id _), (Id _)) -> true
            | ((Int _), (Int _)) -> true
            | (_,_) -> (t1 = t2) in
    let test_map (errors: string) (case: string * rtoken list) : string =
        let(code_string, tkns) = case in
        let cs = explode code_string in
        let head_token = (List.hd tkns) in
            match (p cs) with
                | Cons((tkn,_),_) ->
                      if ((tkn_match head_token expected_token) && 
                              (tkn = head_token)) ||
                          (not(tkn_match head_token expected_token) && 
                                (tkn <> head_token))
                      then errors
                      else errors ^ "\n\tExpected: " ^ (tkn2str(head_token)) ^ 
                          " Lexed: " ^ (tkn2str(tkn))
                | Nil ->
                      if (tkn_match head_token expected_token)
                      then errors ^ "\n\tReturned no token but expected: " ^
                          (tkn2str(head_token))
                      else errors in
    let result = List.fold_left test_map "" lex_test_inputs in
        if (result <> "")
        then Verbose_Test(label, (fun () -> (false, "Lexing error:" ^ result)))
        else Verbose_Test(label, (fun () -> (true, "Lexed expected tokens")))

(* This test maker setup does not work for testing the individual
   Gt, Lt, Not, Eq, and keyword combinators, due to longest match rule.
   However, complete_combinator tests should validate them. *)
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
let test_lcurly_combinator =
   (mk_lex_combinator_test lcurly_combinator (LCurly) "LCurly combinator");;   
let test_rcurly_combinator =
   (mk_lex_combinator_test rcurly_combinator (RCurly) "RCurly combinator");;  

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
                          "\nLexed: " ^ (tkn2str(tkn))
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
            else errors ^ "\nExpected:" ^ (tkn_list2str tkns) ^ "\nLexed:   " ^
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

run_test_set [test_int_combinator;
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
              test_lcurly_combinator;
              test_rcurly_combinator;
              test_complete_combinator;] "Token Combinator Tests";;
run_test_set [test_tokenizer_snippets] "Tokenizer Tests";;
*)
