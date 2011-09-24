(* This file should be extended to implement the Fish parser using the 
 * parsing combinator library, and the combinator-based lexer. *)
open Lcombinators.GenericParsing
open Comblexer
open Ast

exception TODO
exception InvalidSyntax
    
(* Helpful parsers *)
let token_equal(target_token: rtoken) : (token, token) parser =
    (satisfy (fun t_token -> 
                let subrtoken = get_token_rtoken t_token in 
                subrtoken = target_token ))

(* Function packaging If Statement           *)         
let pkg_if (target : (token * (exp * (stmt * (token * stmt) option)))) : stmt = 
    let position = get_token_position (fst target) in
    match target with 
    | (_, (t_expr, (s_then, Some(_, s_else)))) -> (If(t_expr, s_then, s_else), position)
    | (_, (t_expr, (s_then, None)))            -> (If(t_expr, s_then, (skip, position)),   position)

(* Function packaging Return Statement       *)                                                                                          
let pkg_return (target : (token * exp)) : stmt =
    let position = get_token_position (fst target) in
    match target with 
    | (_, t_expr) -> (Return(t_expr), position)
    
(* Function packaging While Statement        *)
let pkg_while (target : (token * (exp * stmt))) : stmt = 
    let position = get_token_position (fst target) in                      
    match target with 
    | (_, (t_expr, t_stmt)) -> (While(t_expr, t_stmt), position)

(* Function packaging Blocks of Statement    *)
let pkg_seq (target : (token * (stmt list * token))) : stmt =
    match target with 
    | (_, (stmts, _)) -> (List.fold_left 
                               (fun (sequence : stmt) (elt : stmt) ->
                                    let position = get_stmt_position elt in                      
                                    (Seq(elt, sequence)), position)
                               (skip, get_token_position (fst target))
                               stmts)

(* Function to package Int-init parse_expression    *) 
let pkg_int_init (target : (token * (token * exp) option)) : exp = 
    let position = get_token_position (fst target) in
    match target with
    | ((Comblexer.Int(num), _), Some((Comblexer.Plus,  _), t_expr)) -> (Binop((Int(num), position), Plus,  t_expr), position)
    | ((Comblexer.Int(num), _), Some((Comblexer.Minus, _), t_expr)) -> (Binop((Int(num), position), Minus, t_expr), position)
    | ((Comblexer.Int(num), _), Some((Comblexer.Times, _), t_expr)) -> (Binop((Int(num), position), Times, t_expr), position)
    | ((Comblexer.Int(num), _), Some((Comblexer.Div,   _), t_expr)) -> (Binop((Int(num), position), Div,   t_expr), position)
    | ((Comblexer.Int(num), _), Some((Comblexer.Gt,    _), t_expr)) -> (Binop((Int(num), position), Gt,    t_expr), position)
    | ((Comblexer.Int(num), _), Some((Comblexer.Gte,   _), t_expr)) -> (Binop((Int(num), position), Gte,   t_expr), position)
    | ((Comblexer.Int(num), _), Some((Comblexer.Lte,   _), t_expr)) -> (Binop((Int(num), position), Lte,   t_expr), position)
    | ((Comblexer.Int(num), _), Some((Comblexer.Lt,    _), t_expr)) -> (Binop((Int(num), position), Lt,    t_expr), position)
    | ((Comblexer.Int(num), _), Some((Comblexer.Eq,    _), t_expr)) -> (Binop((Int(num), position), Eq,    t_expr), position)
    | ((Comblexer.Int(num), _), Some((Comblexer.Neq,   _), t_expr)) -> (Binop((Int(num), position), Neq,   t_expr), position)
    | ((Comblexer.Int(num), _), Some((Comblexer.Or,    _), t_expr)) -> (Or((Int(num), position), t_expr), position)
    | ((Comblexer.Int(num), _), Some((Comblexer.And,   _), t_expr)) -> (And((Int(num), position), t_expr), position)
    | ((Comblexer.Int(num), _), None)                               -> (Int(num), position)
    | _                                                             -> raise InvalidSyntax
    
(* Function to package Var-init parse_expression    *) 
let pkg_var_init (target : (token * (token * exp) option)) : exp = 
   match target with
   | (Comblexer.Var(name), Some(Comblexer.Plus, t_expr))   ->   Plus(Var(name), t_expr) 
   | (Comblexer.Var(name), Some(Comblexer.Minus, t_expr))  ->    Sub(Var(name), t_expr)
   | (Comblexer.Var(name), Some(Comblexer.Times, t_expr))  ->  Times(Var(name), t_expr)
   | (Comblexer.Var(name), Some(Comblexer.Div, t_expr))    ->    Div(Var(name), t_expr)
   | (Comblexer.Var(name), Some(Comblexer.Gt, t_expr))     ->     Gt(Var(name), t_expr)
   | (Comblexer.Var(name), Some(Comblexer.Gte, t_expr))    ->    Gte(Var(name), t_expr)
   | (Comblexer.Var(name), Some(Comblexer.Lte, t_expr))    ->    Lte(Var(name), t_expr)
   | (Comblexer.Var(name), Some(Comblexer.Lt, t_expr))     ->     Lt(Var(name), t_expr)
   | (Comblexer.Var(name), Some(Comblexer.Eq, t_expr))     ->     Eq(Var(name), t_expr)
   | (Comblexer.Var(name), Some(Comblexer.Neq, t_expr))    ->    Neq(Var(name), t_expr)
   | (Comblexer.Var(name), Some(Comblexer.Assign, t_expr)) -> Assign(Var(name), t_expr)
   | (Comblexer.Var(name), None)                           ->        Var(name)


(* Function packaging For Statement          *)        
let pkg_for (target : (token * (token * 
                        (exp * (token * (exp * (token * (exp * 
                        (token * stmt) ))))))))
            : stmt = 
    let position = get_token_position (fst target) in 
    match target with 
    | (_, (_, (i_expr, (_, (c_expr, (_, (n_expr, (_, t_stmt)))))))) ->
            (For(i_expr, c_expr, n_expr, t_stmt), position)    

(* Function packaging expressions -> stmt     *)
let pkg_s_expression (target : exp) : stmt = 
    (Exp(target), get_exp_position target)


(* Parser matching Expressions                *)
let rec parse_expression : (token, exp) parser = 
    (alts [ parse_int_init; 
            parse_var_init; 
            parse_paren_expr ])

(* Statement Parsers *)
and parse_statement : (token, stmt) parser =
    (alts [ parse_if;
            parse_for;
            parse_while;
            parse_return;
            parse_seq;
            parse_s_expression
          ] )
    
(* Parser matching Return Statement          *) 
and parse_return : (token, stmt) parser = 
    (map pkg_return 
         (seq 
             ((token_equal Comblexer.Return ), 
             parse_expression) ))

(* Parser matching If Statement              *)
and parse_if : (token, stmt) parser = 
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
   
(* Parser matching While Statement           *) 
and parse_while : (token, stmt) parser = 
    (map pkg_while 
         (seq 
             (token_equal Comblexer.While)
             (seq 
                 parse_expression
                 parse_statement) ) ) 

(* Parser matching For Statement             *)
(* TODO implement mapping to get correct types *)
and parse_for : (token, stmt) parser = 
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

(* Parser matching Blocks of Statement { x } *) 
and parse_seq : (token, stmt) parser = 
    (map pkg_seq
         (seq 
             (token_equal Comblexer.LCurly)
             (seq 
                 (star parse_statement)
                 token_equal Comblexer.RCurly 
             )))
    
(* Parser pushing an isolated expr -> stmt    *)
and parse_s_expression : (token, stmt) parser =
    (map pkg_s_expression 
         parse_expression)
            
(* Expression Parsers *) 

(* Parameterized Parser for      [binop] expr *) 
and parse_half_binop(operation : token) : (token, (token * exp)) parser = 
    (seq 
        (token_equal operation)
        parse_expression)

(* Parser for 2nd half Plus operation + expr  *)
and parse_half_plus : (token, (token * exp)) parser =
    (parse_half_binop Comblexer.Plus)

(* Parser for 2nd half Times operation + expr *)
and parse_half_sub : (token, (token * exp)) parser =
    (parse_half_binop Comblexer.Minus)

(* Parser for 2nd half Div operation + expr   *)
and parse_half_times : (token, (token * exp)) parser =
    (parse_half_binop Comblexer.Times)

(* Parser for 2nd half Sub operation + expr   *)
and parse_half_div : (token, (token * exp)) parser =
    (parse_half_binop Comblexer.Div)

(* Parser for 2nd half assign operation       *)
and parse_half_assign : (token, (token * exp)) parser =
    (parse_half_binop Comblexer.Assign)

(* Parser for 2nd half lt operation + expr  *)
and parse_half_lt : (token, (token * exp)) parser = 
    (parse_half_binop Comblexer.Lt)

(* Parser for 2nd half lte operation + expr *)
and parse_half_lte : (token, (token * exp)) parser = 
    (parse_half_binop Comblexer.Lte)

(* Parser for 2nd half gt operation + expr   *)
and parse_half_gt : (token, (token * exp)) parser = 
    (parse_half_binop Comblexer.Gt)

(* Parser for 2nd half gte operation + expr   *)
and parse_half_gte : (token, (token * exp)) parser = 
    (parse_half_binop Comblexer.Gte)

(* Parser for 2nd half neq operation + expr   *)
and parse_half_neq : (token, (token * exp)) parser = 
    (parse_half_binop Comblexer.Neq)

(* Parser for 2nd half eq operation + expr   *)
and parse_half_eq : (token, (token * exp)) parser = 
    (parse_half_binop Comblexer.Eq)


(* Parser for an Int-initiated parse_expression     *)
and parse_int_init (token, exp) parser = 
   (map pkg_int_init
        (seq
            (satisfy 
                (fun t_token ->
                    match t_token with 
                    | Comblexer.Int(_) -> true
                    | _                -> false 
                ))
            (opt
                (alts 
                 [ parse_half_plus;
                   parse_half_times;
                   parse_half_div;
                   parse_half_sub;
                   parse_half_lte;
                   parse_half_lt;
                   parse_half_eq;
                   parse_half_neq;
                   parse_half_gt;
                   parse_half_gte;
                 ] )
            ) ))

(* Parser for a Var-initiated parse_expression      *)
and parse_var_init : (token, exp) parser =
    (map pkg_var_init
         (seq
            (satisfy 
                (fun t_token ->
                    match t_token with 
                    | Comblexer.Var(_) -> true
                    | _                -> false 
                ))
             (opt 
                 (alts 
                  [ parse_half_plus;
                    parse_half_times;
                    parse_half_div;
                    parse_half_sub;
                    parse_half_lte;
                    parse_half_lt;
                    parse_half_eq;
                    parse_half_neq;
                    parse_half_gt;
                    parse_half_gte;
                    parse_half_assign;
                  ] )
             )))

(* Function to package a paren'd parse_expression   *) 
and pkg_paren_expr target = 
    raise TODO

(* Parser for Paren-contained parse_expression      *)
and parse_paren_expr : (token, exp) parser = 
    (map pkg_paren_expr 
         (seq 
             (token_equal Comblexer.LParen)
             (seq
                 parse_expression
                 (token_equal Comblexer.RParen)
             )))

     

let rec parse(ts:token list) : program = 
    raise ImplementMe
