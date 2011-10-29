open Ast
open Eval

(* This magic is used to glue the generated lexer and parser together.
 * Expect one command-line argument, a file to parse.
 * You do not need to understand this interaction with the system. *)
let parse_file () =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-compile]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Parse.program Lex.lexer (Lexing.from_channel ch)

let compile_prog prog =
  Compile.result2string (Compile.compile prog )

(* Usage ps4 [file-to-compile] *)
let _ =
  let prog = parse_file() in
  print_string (compile_prog prog) 
