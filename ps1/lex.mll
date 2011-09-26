(* Lexer for Fish --- TODO 
Aaron Watanabe and Anand Gupta
*)

(* You need to add new definition to build the
 * appropriate terminals to feed to parse.mly.
 *)

{
open Parse
open Lexing

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }
}

(* definition section *)
let cr='\013'
let nl='\010'
let eol=(cr nl|nl|cr)
let ws=('\012'|'\t'|' ')*
let digit=['0'-'9'] 
let identifier = ['a'-'z' 'A'-'Z'] (['a'-'z' 'A'-'Z' '0'-'9' '_'])*

(* rules section *)
rule lexer = parse
    (* Whitespace *)
    | eol                  { incr_lineno lexbuf; lexer lexbuf } 
    | ws+                  { lexer lexbuf }
    | "/*"                 { comment lexbuf }
    (* Keywords - IDs must come last *)
    | "for"                { FOR }
    | "if"                 { IF }
    | "else"               { ELSE }
    | "while"              { WHILE }
    | "return"             { RETURN }
    | identifier as str    { ID(str) }
    (* INT *)
    | digit+ as num        { INT(int_of_string(num)) } 
    (* Binops *)
    | "+"                  { PLUS }
    | "-"                  { MINUS }
    | "*"                  { TIMES }
    | "/"                  { DIV }
    | eof                  { EOF }
    | "=="                 { EQ }
    | "="                  { ASSIGN }
    | "!="                 { NEQ }
    | ">="                 { GTE }
    | ">"                  { GT }
    | "<="                 { LTE }
    | "<"                  { LT }
    | "&&"                 { AND }
    | "||"                 { OR }
    (* Unops *)
    | "!"                  { NOT }
    (* Deliminators *)
    | ";"                  { SEMI }
    | "("                  { LPAREN }
    | ")"                  { RPAREN }
    | "{"                  { LCURLY }
    | "}"                  { RCURLY }




and comment = parse
    | _                   { comment lexbuf }
    | eof                 { raise (Failure "missing comment terminator") }
    | "*/"                { lexer lexbuf }
