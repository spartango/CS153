open Cish_ast
open Cish_eval

(* This magic is used to glue the generated lexer and parser together.
 * Expect one command-line argument, a file to parse.
 * You do not need to understand this interaction with the system. *)
let parse_file() =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Cish_parse.program Cish_lex.lexer (Lexing.from_channel ch)

let parse_stdin() = 
  Cish_parse.program Cish_lex.lexer (Lexing.from_channel stdin)

let compile_prog prog =
  Cish_compile.result2string (Cish_compile.compile prog )

(* Reads in cish code and evaluates it *)
let _ =
  let prog = parse_stdin() in
  let ans = eval prog in
  print_string ("answer = "^(val2string ans)^"\n")
  (* print_string (compile_prog prog) *)
