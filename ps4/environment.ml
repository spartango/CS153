open Cish_ast
open Utility

let null     = Int(0);;
let env_name = "env";;

let stub_pos = 0;;

(* Environment *)

(* Environment is a LIFO queue (aka Stack), 
 * implemented as a linked List. *)

(* Initializes an environment for a block of code *)
let init_env (code : stmt) : stmt =
  (Let(env_name, null, code), stub_pos)

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
  cish_stmt_from_str ("result = env;" 
                     ^"for(i=0; i<"
                     ^(string_of_int index^"; "
                     ^"i=i+1) result = *(result + 4); "
                     ^"result = *(result);")

(* Modifies the environment to include the marked variable *)
let store_env (value : int) : stmt = 
  cish_stmt_from_str ("result = malloc(8);" 
                     ^"*(result) = "
                     ^(string_of_int value)^"; "
                     ^"*(result+4) = env; "
                     ^"env = result; "
                     )