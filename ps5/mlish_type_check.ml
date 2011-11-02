open Mlish_ast
open Environment 

exception TypeError

let type_error(s:string) = (print_string s; raise TypeError)

let guess () = 
  Guess_t(ref None)

let rec check_exp e env = 
  (* Extract rexp *)
  let (t_rexp, _) = e in
  (* Segregate expressions *)
  match t_rexp with
  | Var(v)                 -> check_var v env
  | PrimApp(p, exps)       -> check_prim p exps env
  | Fn(v, t_exp)           -> check_fn v t_exp env
  | App(t_exp, o_exp)      -> check_app t_exp o_exp env
  | If(cond, t_exp, e_exp) -> check_if cond t_exp e_exp env
  | Let(v, t_exp, in_exp)  -> check_let v t_exp in_exp env

and check_var v env =
  (* Look up var *)
  lookup v env

and check_prim p exps env = 
  (* Match primitive possibilities *)
  (* Raw *)

  (* Int ops *)

  (* Pairs *)

  (* Lists *)

and check_fn v t_exp env = 
  (* Add v to the env as a guess *)
  let arg_type = (guess ()) in
  let new_env = push v arg_type env in
  (* Check body *)
  let body_type = check_exp t_exp in
  (* Return function type *)
  Fn_t(arg_type, body_type) 

and check_app t_exp o_exp env = 
  (* Return type is a guess *)
  (* Type check both expressions *)
  (* Unify first expression as a function that can take the second *)

and unify a_type b_type = 
  (* Compare for easy equality *)
  (* If a_ is not yet assigned, assign it  *)
  (* If a_ is a guess, try to resolve it   *)
  (* If b_ is a guess, use a_ to assign it *)
  (* Recurse if functions involved *)

and check_if cond t_exp e_exp env = 
  (* Check condition for bool *)
    (* If it a guess, try to resolve it *)
    (* If its not assigned, assign it to bool *) 
  (* Check t_exp and e_exp for equality, type *)

and check_let v t_exp in_exp env =
  (* Create a guess for v in env *)
  (* Check t_exp *)
    (* Assign type to the guess *)
  (* Check in_exp with new environemnt *)

let type_check_exp (e:Mlish_ast.exp) : tipe =


    