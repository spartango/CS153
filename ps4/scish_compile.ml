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

let create_closure (arg : string) 
                   (body : exp) 
                   (f_list : func list) 
                   (scope : var list) 
                   : (func list * var list * stmt) =
  (* Allocate Space    *)
  (* Push scope        *)
  (* Generate Function *)
  (* Compile body      *)
  (* Put pointers      *)
  raise Unimplemented

let rec compile_exp_r ( t_expr : Scish_ast.exp ) 
                      ( f_list : func list     ) 
                      ( scope  : var list      )
                      : (func list * var list * stmt) =
  match t_expr with
  | Int(i)            -> (f_list, scope, (Cish_ast.Int(i), 0))
  | Var(v)            -> (* Scope lookup *)
                          let scope_loc = (scope_index v scope) in
                          let code = lookup_env scope_loc in 
                         (* Env lookup   *)
                          (f_list, scope, code)

  | PrimApp(op, exps) -> raise Unimplemented (* TODO: Primitives     *)
  | Lambda(v, t_exp)  -> create_closure v t_expr f_list scope 
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