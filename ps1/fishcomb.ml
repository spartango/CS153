open Ast
open Explode

(* This function takes an IO stream as input and uses it to read
 * a Mini-ML chunk of abstract synatx via the combinator-based 
 * lexer and parser. *)
let parse_stream stream =
  let size = in_channel_length stream in
  let s = String.create size in
  let _ = really_input stream s 0 size in
  let cs = explode s in
  let tokens = Comblexer.tokenize cs in
  let ast = Combparser.parse tokens in 
  ast
    
(* These functions use parse_stream on either a file or a string 
 * and return the abstract syntax tree.  
 *)
let parse_file() = 
  let argv = Sys.argv in
  let _ =
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
    exit 1) in
  let s = open_in argv.(1) in
  let r = parse_stream s in
  let _ = close_in s in
  r

let parse_string s =
  let cs = explode s in
  let tokens = Comblexer.tokenize cs in
  let ast = Combparser.parse tokens in
  ast
                           
(* Expect 1 command line argument, the file to parse
 * usage: ps2comb [file-to-parse] *)
let _ = 
  let prog = parse_file() in
  let ans = Eval.eval prog in
  print_string ("answer = "^(string_of_int ans)^"\n")
