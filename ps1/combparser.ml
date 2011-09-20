(* This file should be extended to implement the Fish parser using the 
 * parsing combinator library, and the combinator-based lexer. *)
open Lcombinators.GenericParsing
open Comblexer
open Ast

(* Module for parsing Fish *)
module FishParsing =
    struct
        open Lcombinators.GenericParsing
        
        (* Statement Parsers *)
        
        (* Function packaging If statements           *)
        
        (* Parser matching If statements              *)
        
        (* Function packaging Return statements       *)
                                                                                                  
        (* Parser matching Return statements          *) 
        
        (* Function packaging While statements        *)
                                                                                                  
        (* Parser matching While statements           *) 
        
        (* Function packaging For statements          *)        
                                                                                                  
        (* Parser matching For statements             *)
        
        (* Function packaging Blocks of statements    *)
        
        (* Parser matching Blocks of statements { x } *) 
        
        (* Parser matching Expressions                *)
        
        (* Expression Parsers *) 
        
        (* Parameterized Parser for      [binop] expr *) 
        
        (* Parser for 2nd half Plus operation + expr  *)
        
        (* Parser for 2nd half Times operation + expr *)
        
        (* Parser for 2nd half Div operation + expr   *)
         
        (* Parser for 2nd half Sub operation + expr   *)
        
        (* Function to package Int-init expression    *) 
        
        (* Parser for an Int-initiated expression     *)
        
        (* Function to package Var-init expression    *) 

        (* Parser for a Var-initiated expression      *)
        
        (* Function to package a paren'd expression   *) 
        
        (* Parser for Paren-contained expression      *)
                
end
        

let rec parse(ts:token list) : program = 
    raise ImplementMe
