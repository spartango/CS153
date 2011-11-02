open Mlish_ast

exception Not_Found

type environment = (var * tipe) list

(* Look up from head to tail (LIFO stack) *)
let rec lookup v env = 
  match env with 
  | []          -> raise Not_Found
  | (name, t_type)::rest -> if name = v 
                              then t_type 
                              else (lookup v rest)

(* Pushes an element on top of the stack (LIFO) *)
let push v t_type env = 
  (v, t_type)::env

let empty_env = []