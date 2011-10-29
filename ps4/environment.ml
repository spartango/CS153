open Cish_ast
open Utility

exception Not_Found_In_Scope


let env_name = "env";;

(* Environment *)

(* Environment is a LIFO queue (aka Stack), 
 * implemented as a linked List. *)


(* Initializes an environment for a block of code *)
let init_env (code : stmt) : stmt =
    init_var "env" code

(*
 * Each element of the list is a heap-allocated, two-word block
 * which represents a tuple of the form (value, pointer)
 * Values in the environment are immutable, instead they can 
 * only be Shadowed.
 * Variables are indexed in the environment using DeBruijn indicies
 * allowing for O(n) lookup by seeking
 *)

(* Looks up a variable (by index) in the environment 
 * and puts it in result. Hops across links
 *)
let lookup_env (index : int) : stmt =
  init_var "i" (zip_cish_strs [ "result = env;"; 
                   "for(i=0; i<" ^(string_of_int index)^"; " ^"i=i+1) result = *(result + 4); ";
                   "result = *(result);"])

(* Modifies the environment to include the marked variable *)
let store_env (value : int) : stmt = 
  zip_cish_strs ["result = malloc(8);"; 
                 "*(result) = " ^ (string_of_int value) ^";";
                 "*(result+4) = env;";
                 "env = result;"]

(* Pushes a variable onto our virtual stack used during code gen *) 
let push_scope (varname : string) (scope : string list) : string list = 
  [varname;] @ scope

(* Pops a variable off our virtual stack used during code gen *) 
let pop_scope (scope : string list) =
  match scope with 
  | []         -> []
  | head::rest -> rest

(* Looks up a variable in our virtual stack used during code gen *) 
let scope_index (varname : string) (t_scope : var list) : int =
  let rec scope_index_r current_index c_scope =
    match c_scope with 
    | []         -> raise Not_Found_In_Scope
    | head::rest -> if head = varname 
                      then current_index
                      else scope_index_r (current_index+1) rest
  in
  scope_index_r 0 t_scope
  
                                       
