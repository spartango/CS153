(* This file should be extended to implement the Fish parser using the 
 * parsing combinator library, and the combinator-based lexer. *)
open Lcombinators.GenericParsing
open Comblexer
open Ast

(* Module for parsing Fish *)
module FishParsing =
    struct
        open GenericParsing
        
        (* Statement Parsers *)
        
        (* Parser matching If statements              *)
                                                      
        (* Parser matching Return statements          *) 
                                                      
        (* Parser matching While statements           *) 
                                                      
        (* Parser matching For statements             *)
        
        (* Parser matching Blocks of statements { x } *) 
        
        (* Parser matching Expression statements      *)
        
        (* Expression Parsers *) 
        
        (* Parameterized Parser for Int [binop] expr *) 
        
        (* Parameterized Parser for Var [binop] expr *)
        
        (* Parser for Not expr                       *)
        
        (* Parser for Paren-contained expression     *)
                
end
        

let rec parse(ts:token list) : program = 
    raise ImplementMe
