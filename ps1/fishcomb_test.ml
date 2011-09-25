open Test_framework
open Lcombinators.GenericParsing
open Comblexer
open Ast
open Combparser 

let stub = Test("Implemented", (fun () -> false)  );;
    
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

run_test_set [ test_token_equal;
               test_int_alone;
               test_var_alone;
             ]
            "Parser Building Blocks"
