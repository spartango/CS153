open Cish_ast

let null     = Int(0);;
let env_name = "env";;

(* Environment *)

(* Environment is a LIFO queue (aka Stack), 
 * implemented as a linked List. *)

(* Initializes an environment for a block of code *)
let init_env (code : stmt) : stmt =
  (Let(env_name, null, code), 0)

(*
 * Each element of the list is a heap-allocated, two-word block
 * which represents a tuple of the form (value, pointer)
 * Values in the environment are immutable, instead they can 
 * only be Shadowed.
 * Variables are indexed in the environment using DeBruijn indicies
 * allowing for O(n) lookup by seeking
*)

(* Looks up a variable (by index) in the environment *)
let lookup_env (index : int) : stmt =
  ( For( ( Assign("i", 
                  (Int(0), 0) )
          , 0 ), 
          ( Binop((Var("i"), 0)
                  Lt, 
                  (Int(index), 0) )
          , 0), 
          ( Assign("i", 
                  ( Binop((Var("i"), 0)
                          Plus, 
                          (Int(1), 0) )
                  , 0) )
          , 0 )
       )
  ) 
