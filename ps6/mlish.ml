open Mlish_ast

(* This magic is used to glue the generated lexer and parser together.
 * Expect one command-line argument, a file to parse.
 * You do not need to understand this interaction with the system. *)
let parse_file () =
  let argv = Sys.argv in
  let _ = 
    if ((Array.length argv < 2) || (Array.length argv > 3))
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-optimize] [thresh]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Ml_parse.program Ml_lex.lexer (Lexing.from_channel ch)

let compile_prog thresh prog = 
  let t = Mlish_type_check.type_check_exp prog in
  let _ = print_string ("Type: "^Mlish_ast.t2s t^"\n") in
  let sc = Mlish_compile.compile_exp prog in
  let _ = print_string "Scheme Code:\n" in
  let _ = print_string (Scish_ast.exp2string sc) in
  let m = Monadic.tomonadic sc in
  let _ = print_string "\nInitial Monadic Code\n" in
  let _ = print_string (Monadic.exp2string m) in
  let m2 = Monadic.optimize thresh m in
  let _ = print_string "\nOptimized Monadic Code\n" in
  print_string ((Monadic.exp2string m2)^"\n")

let run_prog prog = Scish_eval.run prog

let _ = 
  let prog = parse_file() in
  if Array.length Sys.argv = 3
  then
    (compile_prog (Monadic.size_inline_thresh (int_of_string Sys.argv.(2))) prog)
  else
    (compile_prog Monadic.always_inline_thresh prog)
