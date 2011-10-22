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

let result_name = "result";;

let rec compile_exp_r (t_expr : Scish_ast.exp) ( f_list : func list ) 
                      : (func list * stmt) =
  match t_expr with
  | Int(i)
  | Var(v)
  | PrimApp(op, exps)
  | Lambda(v, t_exp)
  | App(e1, e2)
  | If(e1, e2, e3)

let init_result (code : stmt) : stmt =
  (Let(result_name, (null, stub_pos), code), stub_pos)

(* Initialize an environment    *)
(* Env starts as a null pointer *)
let init (t_expr : Scish_ast.exp) : (func list * stmt) =
  let (fns, code) = compile_exp_r t_expr [] in
  let new_code    = init_result   code      in
  let new_code    = init_env      new_code  in
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