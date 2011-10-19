open Cish_ast

(* Environment *)

(* Environment is a LIFO queue (aka Stack), 
 * implemented as a linked List. *)

(*
 * Each element of the list is a heap-allocated, two-word block
 * which represents a tuple of the form (value, pointer)
 * Values in the environment are immutable, instead they can 
 * only be Shadowed.
 * Variables are indexed in the environment using DeBruijn indicies
 * allowing for O(n) lookup by seeking
*)