open Scish_ast
open Scish_eval

(* This magic is used to glue the generated lexer and parser together.
 * Expect one command-line argument, a file to parse.
 * You do not need to understand this interaction with the system. *)
let parse_file () =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Scish_parse.program Scish_lex.lexer (Lexing.from_channel ch)

let compile_prog prog = Scish_compile.compile_exp prog

let run_prog prog = Scish_eval.run prog

let dump p = print_string (Cish_ast.prog2string p)

let _ =
  let prog = parse_file() in
(*
let _ =  print_string ("answer = "^(string_of_int ans)^"\n") in
*)
  dump (compile_prog prog)
