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

let run_prog() =
  let prog = parse_file() in
  print_string (compile_prog prog)

let eval_prog() =
  let prog = parse_file() in
  let ans = eval prog in
  print_string ("answer = "^(string_of_int ans)^"\n")

(* This program expects one argument, a file to compile. Use run_prog to 
 * print the generated mips code to stdout. Comment run_prog and uncomment 
 * eval_prog to get the evaluated answer instead. By default, this is set 
 * up to print mips code. Usage: ps3 [file-to-parse] *)
let _ = run_prog()
(* let _ = eval_prog() *)
