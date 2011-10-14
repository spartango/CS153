(* TODO:  your job is to map ScishAst expressions to CishAst functions. 
   The file sample_input.scish shows a sample Scish expression and the
   file sample_output.cish shows the output I get from my compiler.
   You will want to do your own test cases...
 *)

exception Unimplemented

let rec compile_exp (e:Scish_ast.exp) : Cish_ast.program = raise Unimplemented
