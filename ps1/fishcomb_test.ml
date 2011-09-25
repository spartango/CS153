open Test_framework
open Lcombinators.GenericParsing
open Comblexer
open Ast
open Combparser 
    
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
            let input_tokens = [ (Comblexer.Var("x"), 0);] in
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
                [ (LParen, 0); (Comblexer.Var("x"), 0); (RParen, 0);] in
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
                [ (Comblexer.Var("x"), 0); 
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
                [ (Comblexer.Var("x"), 0); 
                  (Comblexer.Plus, 0);
                  (Comblexer.Var("x"), 0);] in
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
                [ (Comblexer.Var("x"), 0); 
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
                [ (Comblexer.Var("x"), 0); 
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
                [ (Comblexer.Var("x"), 0); 
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
                  (Comblexer.Var("x"), 0);] in
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
                  (Comblexer.Var("x"), 0); 
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
                  (Comblexer.Var("x"), 0); 
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
                  (Comblexer.Var("x"), 0); 
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
                  (Comblexer.Var("x"), 0); 
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
                     (Comblexer.Var("x"), 0);
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
                     (Comblexer.Var("x"), 0);
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
                     (Comblexer.Var("x"), 0);
                     (Comblexer.Plus, 0);
                     (Comblexer.Int(1), 0);
                     (Comblexer.Seq, 0);
                     (Comblexer.Var("y"), 0);
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
                    (Comblexer.Var("x"), 0);
                    (Comblexer.Seq, 0);
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
                    (Comblexer.Var("x"), 0);
                    (Comblexer.Seq, 0);
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
                    (Comblexer.Var("x"), 0);
                    (Comblexer.Seq, 0);
                    (Comblexer.Else, 0);
                    (Comblexer.Var("y"), 0);
                    (Comblexer.Seq, 0);
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
                     (Comblexer.Var("i"), 0);
                     (Comblexer.Assign, 0);
                     (Comblexer.Int(0), 0);
                     (Comblexer.Seq, 0);
                     (Comblexer.Var("i"), 0);
                     (Comblexer.Eq, 0);
                     (Comblexer.Int(1), 0);
                     (Comblexer.Seq, 0);
                     (Comblexer.Var("i"), 0);
                     (Comblexer.Assign, 0);
                     (Comblexer.Var("i"), 0);
                     (Comblexer.Plus, 0);
                     (Comblexer.Int(1), 0);
                     (Comblexer.RParen, 0);
                     (Comblexer.Var("i"), 0);
                     (Comblexer.Seq, 0)
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

             
