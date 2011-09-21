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
        let parse_statement : (token, stmt) parser =
            (alts [ parse_if;
                    parse_for;
                    parse_while;
                    parse_return;
                    parse_s_expression;
                    parse_seq
                  ] )

        (* Function packaging If Statement           *)         
        let pkg_if (target : (token * (exp * (stmt * (token * stmt) option)))) : stmt = 
            match target with 
            | (_, (t_expr, (s_then, Some(_, s_else)))) -> If(t_expr, s_then, s_else)
            | (_, (t_expr, (s_then, None)))            -> If(t_expr, s_then, skip)

        (* Parser matching If Statement              *)
        let parse_if : (token, stmt) parser = 
            (map pkg_if
                 (seq 
                     (token_equal Comblexer.If)
                     (seq 
                         parse_expression
                         (seq 
                             parse_statement
                             (opt 
                                 (seq 
                                     (token_equal Comblexer.Else)
                                     parse_statement
                                 ))))))

        (* Function packaging Return Statement       *)                                                                                          
        let pkg_return (target : (token * exp)) : stmt =
            match target with 
            | (_, t_expr) -> Return(t_expr)

        (* Parser matching Return Statement          *) 
        (* TODO fix return types in seq *)
        let parse_return : (token, stmt) parser = 
            (map pkg_return 
                 (seq 
                     (token_equal Comblexer.Return) 
                     parse_expression ) ) 
            
        (* Function packaging While Statement        *)
        let pkg_while (target : (token * (exp * stmt))) : stmt =                           
            match target with 
            | (_, (t_expr, t_stmt)) -> While(t_expr, t_stmt)

        (* Parser matching While Statement           *) 
        let parse_while : (token, stmt) parser = 
            (map pkg_while 
                 (seq 
                     (token_equal Comblexer.While)
                     (seq 
                         parse_expression
                         parse_statement) ) ) 

        (* Function packaging For Statement          *)        
        let pkg_for (target : (token * (token * 
                                (exp * (token * (exp * (token * (exp * 
                                (token * stmt) ))))))) 
                    : stmt = 
            match target with 
            | (_, (_, (i_expr, (_, (c_expr, (_, (n_expr, (_, t_stmt)))))))) ->
                    For(i_expr, c_expr, n_expr, t_stmt)

        (* Parser matching For Statement             *)
        (* TODO implement mapping to get correct types *)
        let parse_for : (token, stmt) parser = 
            (map pkg_for
                 (seq
                     (token_equal Comblexer.For) 
                     (seq 
                         (token_equal Comblexer.LParen)
                         (seq
                             parse_expression
                             (seq 
                                 (token_equal Comblexer.Seq) 
                                 (seq 
                                     parse_expression 
                                     (seq
                                         (token_equal Comblexer.Seq)
                                         (seq
                                             parse_expression
                                             (seq
                                                 (token_equal Comblexer.RParen)
                                                 parse_statement
                                             )))))))))

        (* Function packaging Blocks of Statement    *)
        let pkg_seq (target : (token * (stmt list * token))) : stmt =
            match target with 
            | (_, (stmts, _)) -> (List.fold_left 
                                       (fun sequence elt ->
                                            Seq(elt, sequence)) 
                                       (always ())
                                       stmts)

        (* Parser matching Blocks of Statement { x } *) 
        let parse_seq (token, stmt) parser = 
            (map pkg_seq
                 (seq 
                     (token_equal Comblexer.LCurly)
                     (seq 
                         (star parse_statement)
                         token_equal Comblexer.RCurly 
                     )))

        (* Parser matching Expressions                *)
        
        (* Expression Parsers *) 
        
        (* Parameterized Parser for      [binop] expr *) 
        
        (* Parser for 2nd half Plus operation + expr  *)
        
        (* Parser for 2nd half Times operation + expr *)
        
        (* Parser for 2nd half Div operation + expr   *)
         
        (* Parser for 2nd half Sub operation + expr   *)
        
        (* Parser for 2nd half assign operation       *)

        (* Function to package Int-init parse_expression    *) 
        let pkg_int_init target = 
            raise TODO
        
        (* Parser for an Int-initiated parse_expression     *)
        let parse_int_init (token, exp) parser = 
           (map pkg_int_init
                (seq
                    (satisfy 
                        (fun t_token ->
                            match t_token with 
                            | Comblexer.Int(_) -> true
                            | _                -> false 
                        ))
                    (alts 
                         [ parse_half_plus;
                           parse_half_times;
                           parse_half_div;
                           parse_half_sub;
                           always ()
                         ]
                    ) ))

        (* Function to package Var-init parse_expression    *) 
        let pkg_var_init target = 
            raise TODO

        (* Parser for a Var-initiated parse_expression      *)
        let parse_var_init : (token, exp) parser =
            (map pkg_var_init
                 (seq
                    (satisfy 
                        (fun t_token ->
                            match t_token with 
                            | Comblexer.Var(_) -> true
                            | _                -> false 
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

        (* Function to package a paren'd parse_expression   *) 
        let pkg_paren_expr target = 
            raise TODO

        (* Parser for Paren-contained parse_expression      *)
        let parse_paren_expr : (token, exp) parser = 
            (map pkg_paren_expr 
                 (seq 
                     (token_equal Comblexer.LParen)
                     (seq
                         parse_expression
                         (token_equal Comblexer.RParen)
                     )))

end
        

let rec parse(ts:token list) : program = 
    raise ImplementMe
