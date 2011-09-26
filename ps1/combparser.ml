(* This file should be extended to implement the Fish parser using the 
 * parsing combinator library, and the combinator-based lexer. *)
open Lcombinators.GenericParsing
open Comblexer
open Ast

exception TODO
exception IntInvalidSyntax
exception ParenInvalidSyntax
exception VarInvalidSyntax
exception NoParses

(* Helpful parsers *)
let token_equal(target_token: rtoken) : (token, token) parser =
    (satisfy (fun t_token -> 
                let subrtoken = get_token_rtoken t_token in 
                subrtoken = target_token ))

let pkg_stmt (target : (stmt * token option)) : stmt = 
    (fst target)

let pkg_empty (target : token) : stmt =
    (skip, (get_token_position target))

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
                                    (Seq(sequence, elt)), position)
                               (skip, get_token_position (fst target))
                               stmts)

(* Function to package a paren'd parse_expression   *) 
let pkg_paren_expr (target : (((token * (exp * token)) * (token * exp) option))) : exp = 
    match target with 
    | (((_, position), (t_exp, _)), Some((Comblexer.Plus,  _), s_expr)) -> (Binop(t_exp, Plus,  s_expr), position)
    | (((_, position), (t_exp, _)), Some((Comblexer.Minus, _), s_expr)) -> (Binop(t_exp, Minus, s_expr), position)
    | (((_, position), (t_exp, _)), Some((Comblexer.Times, _), s_expr)) -> (Binop(t_exp, Times, s_expr), position)
    | (((_, position), (t_exp, _)), Some((Comblexer.Div,   _), s_expr)) -> (Binop(t_exp, Div,   s_expr), position)
    | (((_, position), (t_exp, _)), Some((Comblexer.Gt,    _), s_expr)) -> (Binop(t_exp, Gt,    s_expr), position)
    | (((_, position), (t_exp, _)), Some((Comblexer.Gte,   _), s_expr)) -> (Binop(t_exp, Gte,   s_expr), position)
    | (((_, position), (t_exp, _)), Some((Comblexer.Lte,   _), s_expr)) -> (Binop(t_exp, Lte,   s_expr), position)
    | (((_, position), (t_exp, _)), Some((Comblexer.Lt,    _), s_expr)) -> (Binop(t_exp, Lt,    s_expr), position)
    | (((_, position), (t_exp, _)), Some((Comblexer.Eq,    _), s_expr)) -> (Binop(t_exp, Eq,    s_expr), position)
    | (((_, position), (t_exp, _)), Some((Comblexer.Neq,   _), s_expr)) -> (Binop(t_exp, Neq,   s_expr), position)
    | (((_, position), (t_exp, _)), Some((Comblexer.Or,    _), s_expr)) -> (Or(   t_exp, s_expr), position)
    | (((_, position), (t_exp, _)), Some((Comblexer.And,   _), s_expr)) -> (And(  t_exp, s_expr), position)
    | (((_, position), (t_exp, _)), Some(_))                           -> raise ParenInvalidSyntax
    | (((_, position), (t_exp, _)), None) -> t_exp

(* Function to package a Not expression *)
let pkg_not_init (target : (token * exp)) : exp = 
    let position = get_token_position (fst target) in                      
    match target with 
    | (_, t_exp) -> (Not(t_exp), position)
    
(* Function to package Int-init parse_expression    *) 
let pkg_int_init (target : (token option * (token * (token * exp) option))) : exp = 
    let sign = 
            match (fst target) with 
            | Some(_) -> -1
            | None    -> 1
    in 
    match (snd target) with
    | ((Comblexer.Int(num), position), Some((Comblexer.Plus,  _), t_expr)) -> (Binop((Int(sign * num), position), Plus,  t_expr), position)
    | ((Comblexer.Int(num), position), Some((Comblexer.Minus, _), t_expr)) -> (Binop((Int(sign * num), position), Minus, t_expr), position)
    | ((Comblexer.Int(num), position), Some((Comblexer.Times, _), t_expr)) -> (Binop((Int(sign * num), position), Times, t_expr), position)
    | ((Comblexer.Int(num), position), Some((Comblexer.Div,   _), t_expr)) -> (Binop((Int(sign * num), position), Div,   t_expr), position)
    | ((Comblexer.Int(num), position), Some((Comblexer.Gt,    _), t_expr)) -> (Binop((Int(sign * num), position), Gt,    t_expr), position)
    | ((Comblexer.Int(num), position), Some((Comblexer.Gte,   _), t_expr)) -> (Binop((Int(sign * num), position), Gte,   t_expr), position)
    | ((Comblexer.Int(num), position), Some((Comblexer.Lte,   _), t_expr)) -> (Binop((Int(sign * num), position), Lte,   t_expr), position)
    | ((Comblexer.Int(num), position), Some((Comblexer.Lt,    _), t_expr)) -> (Binop((Int(sign * num), position), Lt,    t_expr), position)
    | ((Comblexer.Int(num), position), Some((Comblexer.Eq,    _), t_expr)) -> (Binop((Int(sign * num), position), Eq,    t_expr), position)
    | ((Comblexer.Int(num), position), Some((Comblexer.Neq,   _), t_expr)) -> (Binop((Int(sign * num), position), Neq,   t_expr), position)
    | ((Comblexer.Int(num), position), Some((Comblexer.Or,    _), t_expr)) -> (Or(   (Int(sign * num), position), t_expr), position)
    | ((Comblexer.Int(num), position), Some((Comblexer.And,   _), t_expr)) -> (And(  (Int(sign * num), position), t_expr), position)
    | ((Comblexer.Int(num), position), None)                               -> (Int(sign * num), position)
    | _                                                                    -> raise IntInvalidSyntax
    
(* Function to package Int-init parse_expression    *) 
let pkg_var_init (target : (token option * (token * (token * exp) option))) : exp = 
    let sign = 
            match (fst target) with 
            | Some(_) -> -1
            | None    -> 1
    in
    match (snd target) with
    | ((Id(name), position), Some((Comblexer.Plus,   _), t_expr)) -> (Binop( (if sign = -1 then (Binop((Int(sign), position), Times, (Var(name), position)), position) else (Var(name), position)), Plus,  t_expr), position)
    | ((Id(name), position), Some((Comblexer.Minus,  _), t_expr)) -> (Binop( (if sign = -1 then (Binop((Int(sign), position), Times, (Var(name), position)), position) else (Var(name), position)), Minus, t_expr), position)
    | ((Id(name), position), Some((Comblexer.Times,  _), t_expr)) -> (Binop( (if sign = -1 then (Binop((Int(sign), position), Times, (Var(name), position)), position) else (Var(name), position)), Times, t_expr), position)
    | ((Id(name), position), Some((Comblexer.Div,    _), t_expr)) -> (Binop( (if sign = -1 then (Binop((Int(sign), position), Times, (Var(name), position)), position) else (Var(name), position)), Div,   t_expr), position)
    | ((Id(name), position), Some((Comblexer.Gt,     _), t_expr)) -> (Binop( (if sign = -1 then (Binop((Int(sign), position), Times, (Var(name), position)), position) else (Var(name), position)), Gt,    t_expr), position)
    | ((Id(name), position), Some((Comblexer.Gte,    _), t_expr)) -> (Binop( (if sign = -1 then (Binop((Int(sign), position), Times, (Var(name), position)), position) else (Var(name), position)), Gte,   t_expr), position)
    | ((Id(name), position), Some((Comblexer.Lte,    _), t_expr)) -> (Binop( (if sign = -1 then (Binop((Int(sign), position), Times, (Var(name), position)), position) else (Var(name), position)), Lte,   t_expr), position)
    | ((Id(name), position), Some((Comblexer.Lt,     _), t_expr)) -> (Binop( (if sign = -1 then (Binop((Int(sign), position), Times, (Var(name), position)), position) else (Var(name), position)), Lt,    t_expr), position)
    | ((Id(name), position), Some((Comblexer.Eq,     _), t_expr)) -> (Binop( (if sign = -1 then (Binop((Int(sign), position), Times, (Var(name), position)), position) else (Var(name), position)), Eq,    t_expr), position)
    | ((Id(name), position), Some((Comblexer.Neq,    _), t_expr)) -> (Binop( (if sign = -1 then (Binop((Int(sign), position), Times, (Var(name), position)), position) else (Var(name), position)), Neq,   t_expr), position)
    | ((Id(name), position), Some((Comblexer.Or,     _), t_expr)) -> (Or(    (if sign = -1 then (Binop((Int(sign), position), Times, (Var(name), position)), position) else (Var(name), position)),        t_expr), position)
    | ((Id(name), position), Some((Comblexer.And,    _), t_expr)) -> (And(   (if sign = -1 then (Binop((Int(sign), position), Times, (Var(name), position)), position) else (Var(name), position)),        t_expr), position)
    | ((Id(name), position), Some((Comblexer.Assign, _), t_expr)) ->  (Assign(name, t_expr), position)
    | ((Id(name), position), None)                                -> ((if sign = -1 then (Binop((Int(sign), position), Times, (Var(name), position)), position) else (Var(name), position)))
    | _                                                           -> raise VarInvalidSyntax


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
    fun cs ->
    (alts [ parse_int_init; 
            parse_var_init; 
            parse_paren_expr;
            parse_not_init ])
    cs
    
(* Expression Parsers *) 

(* Parameterized Parser for      [binop] expr *) 
and parse_half_binop(operation : rtoken) : (token, (token * exp)) parser = 
    fun cs ->
    (seq 
        ((token_equal operation),
        parse_expression))
    cs
    
(* Parser for an Int-initiated parse_expression     *)
and parse_int_init : (token, exp) parser = 
    fun cs ->
    (map pkg_int_init
        (seq 
            ((opt (token_equal Comblexer.Minus)),
            (seq
                ((satisfy 
                    (fun t_token ->
                        let subrtoken = get_token_rtoken t_token in
                        match subrtoken with 
                        | Comblexer.Int(_) -> true
                        | _                -> false 
                    )),
                (opt
                    (alts 
                     [ 
                       (parse_half_binop Comblexer.Times);
                       (parse_half_binop Comblexer.Div);
                       (parse_half_binop Comblexer.Plus);
                       (parse_half_binop Comblexer.Minus);
                       (parse_half_binop Comblexer.Lte);
                       (parse_half_binop Comblexer.Lt);
                       (parse_half_binop Comblexer.Eq);
                       (parse_half_binop Comblexer.Neq);
                       (parse_half_binop Comblexer.Gt);
                       (parse_half_binop Comblexer.Gte);
                       (parse_half_binop Comblexer.Or);
                       (parse_half_binop Comblexer.And)
                     ] )
                ) )))))
    cs
    
(* Parser for a Var-initiated parse_expression      *)
and parse_var_init : (token, exp) parser =
    fun cs ->
    (map pkg_var_init
        (seq 
            ((opt (token_equal Comblexer.Minus)),
             (seq
                ((satisfy 
                    (fun t_token ->
                        let subrtoken = get_token_rtoken t_token in
                        match subrtoken with
                        | Id(_) -> true
                        | _                -> false 
                    )),
                 (opt 
                     (alts 
                      [    (parse_half_binop Comblexer.Plus);
                           (parse_half_binop Comblexer.Times);
                           (parse_half_binop Comblexer.Div);
                           (parse_half_binop Comblexer.Minus);
                           (parse_half_binop Comblexer.Lte);
                           (parse_half_binop Comblexer.Lt);
                           (parse_half_binop Comblexer.Eq);
                           (parse_half_binop Comblexer.Neq);
                           (parse_half_binop Comblexer.Gt);
                           (parse_half_binop Comblexer.Gte);
                           (parse_half_binop Comblexer.Or);
                           (parse_half_binop Comblexer.And);
                           (parse_half_binop Comblexer.Assign)
                      ] )
             ) )))))
    cs
    
(* Parser for Paren-contained parse_expression      *)
and parse_paren_expr : (token, exp) parser = 
    fun cs ->
    (map pkg_paren_expr 
         (seq 
             ((seq 
                 ((token_equal Comblexer.LParen),
                 (seq
                     (parse_expression,
                     (token_equal Comblexer.RParen))
                 ))),
             (opt
                 (alts 
                  [ (parse_half_binop Comblexer.Plus);
                    (parse_half_binop Comblexer.Times);
                    (parse_half_binop Comblexer.Div);
                    (parse_half_binop Comblexer.Minus);
                    (parse_half_binop Comblexer.Lte);
                    (parse_half_binop Comblexer.Lt);
                    (parse_half_binop Comblexer.Eq);
                    (parse_half_binop Comblexer.Neq);
                    (parse_half_binop Comblexer.Gt);
                    (parse_half_binop Comblexer.Gte);
                    (parse_half_binop Comblexer.Or);
                    (parse_half_binop Comblexer.And)
                  ] )
             ))
        ))
    cs
    
(* Parser for Paren-contained parse_expression      *)
and parse_not_init : (token, exp) parser = 
    fun cs ->
    (map pkg_not_init 
        (seq 
            ((token_equal Comblexer.Not),
            parse_expression)))
    cs
;;

(* Statement Parsers *)
let rec parse_statement: (token, stmt) parser =
    fun cs -> 
        (map pkg_stmt
            (seq(
                (alts [ parse_if;
                        parse_for;
                        parse_while;
                        parse_return;
                        parse_seq;
                        parse_s_expression;
                        parse_empty;
                      ] ), 
                (opt (token_equal Comblexer.Semi))
        )))
    cs

and parse_empty : (token, stmt) parser =
    fun cs -> 
        (map pkg_empty 
            (token_equal Comblexer.Semi))
    cs 

(* Parser matching Return Statement          *) 
and parse_return : (token, stmt) parser = 
    fun cs ->     
        (map pkg_return 
           (seq 
               ((token_equal Comblexer.Return ), 
               parse_expression) ))
           
    cs
    
(* Parser matching While Statement           *) 
and parse_while : (token, stmt) parser = 
    fun cs ->
    (map pkg_while 
       (seq 
           ((token_equal Comblexer.While), 
           (seq 
               (parse_expression,
               parse_statement
           ))))) 
    cs

(* Parser matching If Statement              *)
and parse_if : (token, stmt) parser = 
    fun cs ->
    (map pkg_if
       (seq 
           ((token_equal Comblexer.If),
           (seq 
               (parse_expression,
               (seq 
                   (parse_statement,
                   (opt 
                       (seq 
                           ((token_equal Comblexer.Else),
                           parse_statement)
                       )))))))))
    cs
                       
(* Parser matching For Statement             *)
and parse_for : (token, stmt) parser = 
    fun cs ->
    (map pkg_for
       (seq
           ((token_equal Comblexer.For),
           (seq 
               ((token_equal Comblexer.LParen),
               (seq
                   (parse_expression,
                   (seq 
                       ((token_equal Comblexer.Semi), 
                       (seq 
                           (parse_expression, 
                           (seq
                               ((token_equal Comblexer.Semi),
                               (seq
                                   (parse_expression,
                                   (seq
                                       ((token_equal Comblexer.RParen),
                                       parse_statement)
                                   ))))))))))))))))
    cs
    
(* Parser matching Blocks of Statement { x } *) 
and parse_seq : (token, stmt) parser = 
   fun cs ->
    (map pkg_seq
       (seq 
           ((token_equal Comblexer.LCurly),
           (seq 
               ((star parse_statement),
               token_equal Comblexer.RCurly) 
           ))))
    cs
    
(* Parser pushing an isolated expr -> stmt    *)
and parse_s_expression : (token, stmt) parser =
    fun cs ->
    (map pkg_s_expression 
       parse_expression)
    cs
;;

let rec parse(ts:token list) : program = 
    (* Parse with combinators*)
    let parsed = parse_statement ts in
    (* Pick best parse *)
    match parsed with 
    | Cons((prog, _), _) -> prog
    | Nil -> raise NoParses
;;
