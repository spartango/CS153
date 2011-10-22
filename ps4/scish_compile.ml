(* TODO:  your job is to map ScishAst expressions to CishAst functions. 
   The file sample_input.scish shows a sample Scish expression and the
   file sample_output.cish shows the output I get from my compiler.
   You will want to do your own test cases...
 *)

open Environment 
open Utility
open Cish_ast
open Scish_ast

exception Unimplemented

let dummy_pos = 0;;
let result_name = "result";;
let result_var : Cish_ast.exp = (Cish_ast.Var(result_name), dummy_pos)

let rec compile_exp_r ( t_expr : Scish_ast.exp ) 
                      ( f_list : func list     ) 
                      ( scope  : var list      )
                      : (func list * var list * stmt) =
  match t_expr with
  | Int(i)            -> (f_list, var_list,
                          (Cish_ast.Exp(Cish_ast.Int i, dummy_pos), dummy_pos))
  | Var(v)            -> raise Unimplemented (* TODO: do lookup      *)
  | PrimApp(op, exps) -> 
        (* Maps over expression list, compiling expressions while accumulating function list and scope *)
        let(f_list1, scope1, stmts) = List.fold_left
            (fun (t_f_list, t_scope, t_stmts) t_e ->
                 let (n_f_list, n_scope, n_stmt) = compile_exp_r t_e t_f_list t_scope in
                     (n_f_list, n_scope, n_stmt::stmts))
            (f_list, scope, []) exps in
        let assign value = (f_list1, scope1, Cish_ast.Exp(Cish_ast.Assign(result_var, value), dummy_pos)) in
            match op with
                | Plus   assign Cish_ast.Binop(
                | Minus  (* subtract two ints *)
                | Times  (* multiply two ints *)
                | Div    (* divide two ints *)
                | Cons   (* create a pair *)
                | Fst    (* fetch the 1st component of a pair *)
                | Snd    (* fetch the 2nd component of a pair *)
                | Eq     (* compare two ints for equality *)
                | Lt     (* compare two ints for inequality *)

  | Lambda(v, t_exp)  ->  (* TODO: create closure *)  
  | App(e1, e2)       -> raise Unimplemented (* TODO: call           *)
  | If(e1, e2, e3)    -> raise Unimplemented (* TODO: logical flow   *)




let init_result (code : stmt) : stmt =
  (Let(result_name, (null, stub_pos), code), stub_pos)

(* Initialize an environment    *)
(* Env starts as a null pointer *)
let init (t_expr : Scish_ast.exp) : (func list * stmt) =
  let (fns, code) = compile_exp_r t_expr [] [] in
  let new_code    = init_result   code         in
  let new_code    = init_env      new_code     in
  (fns, new_code)

(* Create a main function       *)
let rec compile_exp (e:Scish_ast.exp) : Cish_ast.program =
  let (functions, main_body) = (init e) in
    ([Fn( { name = "main"; 
            args = []; 
            body = main_body; 
            pos  = 0;
       } )]
     @ functions)
