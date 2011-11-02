(* header section *)
{
open Ml_parse
open Lexing

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }

let inc x = x := (!x) + 1
let dec x = x := (!x) - 1
let commentLevel = ref 0
let commentStart = ref 0
}

(* definition section *)
let cr='\013'
let nl='\010'
let eol=(cr nl|nl|cr)
let ws=('\012'|'\t'|' ')*
let digit=['0'-'9']

let id = ['a'-'z''_']['a'-'z''A'-'Z''0'-'9''_''\'']*

(* rules section *)

rule lexer = parse
| eol { incr_lineno lexbuf; lexer lexbuf }
| ws+ { lexer lexbuf }
| digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) }
| "::" { CONS }
| '~' { TILDE }
| '+' { PLUS }
| '*' { TIMES }
| '-' { MINUS }
| '/' { DIV }
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACKET }
| ']' { RBRACKET }
| '<' { LT }
| "->" { DARROW }
| '=' { EQUALS }
| ',' { COMMA }
| ';' { SEMI }
| "else" { ELSE }
| "false" { FALSE }
| "fun" { FN }
| "fst" { FST }
| "hd" { HD }
| "if" { IF }
| "in" { IN }
| "isnil" { ISNIL }
| "let" { LET }
| "nil" { NIL }
| "snd" { SND }
| "then" { THEN }
| "tl" { TL }
| "true" { TRUE }
| id { ID(Lexing.lexeme lexbuf) }
| "(*" { comment lexbuf }
| eof { EOF }
and comment = parse
| "(*" { inc commentLevel; comment lexbuf }
| eol {incr_lineno lexbuf; comment lexbuf }
| "*)" { dec commentLevel; if (!commentLevel) = 0 then lexer lexbuf else comment lexbuf }
| _ { comment lexbuf }
