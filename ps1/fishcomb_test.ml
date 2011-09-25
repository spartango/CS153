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
run_test_set [test_token_equal;] "Parser Building Blocks"
