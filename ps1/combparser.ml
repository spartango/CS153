(* This file should be extended to implement the Fish parser using the 
 * parsing combinator library, and the combinator-based lexer. *)
open Lcombinators.GenericParsing
open Comblexer
open Ast

exception TODO

(* Module for parsing Fish *)
module FishParsing =
    struct
        open Lcombinators.GenericParsing
        open Comblexer
        open Ast
        
        (* Helpful parsers *)
        let token_equal(target_token: token) : (token, token) parser = 
            (satisfy (fun t_token -> t_token = target_token))

        (* Statement Parsers *)
         
        (* Function packaging If statements           *)         
            
        (* Parser matching If statements              *)
        
        (* Function packaging Return statements       *)                                                                                          
        let pkg_return (target : (token * expr)) : rstmt =
            raise TODO

        (* Parser matching Return statements          *) 
        (* TODO fix return types in seq *)
        let parse_return : (token, rstmt) parser = 
            (map pkg_return 
                 (seq 
                     (token_equal Return) 
                     expression ) ) 
            
        (* Function packaging While statements        *)
        let pkg_while (target : (token * (expr * statement))) : rstmt =                           
            raise TODO

        (* Parser matching While statements           *) 
        let parse_while : (token, rstmt) parser = 
            (map pkg_while 
                 (seq 
                     (token_equal While)
                     (seq 
                         expression
                         statement) ) ) 

        (* Function packaging For statements          *)        
        let pkg_for (target : (token * (expr * (expr * (expr * rstmt))))) 
                    : rstmt = 
            raise TODO

        (* Parser matching For statements             *)
        (* TODO implement mapping to get correct types *)
        let parse_for : (token, rstmt) parser = 
            (map pkg_for
                 (seq
                     (token_equal For) 
                     (seq 
                         (token_equal LParen)
                         (seq
                             expression
                             (seq 
                                 (token_equal Seq) 
                                 (seq 
                                     expression 
                                     (seq
                                         (token_equal Seq)
                                         (seq
                                             expression
                                             (seq
                                                 (token_equal LParen)
                                                 statement
                                             )))))))))

        (* Function packaging Blocks of statements    *)
        let pkg_seq (target : (token * (rstmt list * token))) : rstmt =
            raise TODO

        (* Parser matching Blocks of statements { x } *) 
        let parse_lcurly (token, rstmt) parser = 
            (map pkg_seq
                 (seq 
                     (token_equal LCurly)
                     (seq 
                         (star statement)
                         token_equal RCurly 
                     )))

        (* Parser matching Expressions                *)
        
        (* Expression Parsers *) 
        
        (* Parameterized Parser for      [binop] expr *) 
        
        (* Parser for 2nd half Plus operation + expr  *)
        
        (* Parser for 2nd half Times operation + expr *)
        
        (* Parser for 2nd half Div operation + expr   *)
         
        (* Parser for 2nd half Sub operation + expr   *)
        
        (* Parser for 2nd half assign operation       *)

        (* Function to package Int-init expression    *) 
        let pkg_int_init target = 
            raise TODO
        
        (* Parser for an Int-initiated expression     *)
        let parse_int_init (token, expr) parser = 
           (map pkg_int_init
                (seq
                    (satisfy 
                        (fun t_token ->
                            match t_token with 
                            | Int(_) -> true
                            | _      -> false 
                        ))
                    (alts 
                         [ parse_half_plus;
                           parse_half_times;
                           parse_half_div;
                           parse_half_sub;
                           always ()
                         ]
                    ) ))

        (* Function to package Var-init expression    *) 
        let pkg_var_init target = 
            raise TODO

        (* Parser for a Var-initiated expression      *)
        let parse_var_init : (token, expr) parser =
            (map pkg_var_init
                 (seq
                    (satisfy 
                        (fun t_token ->
                            match t_token with 
                            | Int(_) -> true
                            | _      -> false 
                        ))
                     (alts 
                          [ parse_half_plus;
                            parse_half_sub;
                            parse_half_times;
                            parse_half_div;
                            parse_half_assign;
                            always ()
                          ]
                     )))

        (* Function to package a paren'd expression   *) 
        let pkg_paren_expr target = 
            raise TODO

        (* Parser for Paren-contained expression      *)
        let parse_paren_expr : (token, expr) parser = 
            (map pkg_paren_expr 
                 (seq 
                     (token_equal LParen)
                     (seq
                         expression
                         (token_equal RParen)
                     )))

end
        

let rec parse(ts:token list) : program = 
    raise ImplementMe
