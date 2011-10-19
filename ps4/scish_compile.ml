(* TODO:  your job is to map ScishAst expressions to CishAst functions. 
   The file sample_input.scish shows a sample Scish expression and the
   file sample_output.cish shows the output I get from my compiler.
   You will want to do your own test cases...
 *)

exception Unimplemented

let rec compile_exp (e:Scish_ast.exp) : Cish_ast.program = raise Unimplemented

(* Environment *)

(* Environment is a LIFO queue (aka Stack), 
 * implemented as a linked List.
 * Each element of the list is a heap-allocated, two-word block
 * which represents a tuple of the form (value, pointer)
 * Values in the environment are immutable, instead they can 
 * only be Shadowed.
 * Variables are indexed in the environment using DeBruijn indicies
 * allowing for O(n) lookup by seeking
*)