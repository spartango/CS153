open Ast
open Eval

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
  Parse.program Lex.lexer (Lexing.from_channel ch)

let parse_stdin() =
  Parse.program Lex.lexer (Lexing.from_channel stdin)

(* Expect 1 command line argument, the file to parse 
 * usage: ps2yacc [file-to-parse] *)
let _ =
  let prog = parse_file() in
  let ans = eval prog in
  print_string ("answer = "^(string_of_int ans)^"\n")
