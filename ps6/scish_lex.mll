(* header section *)
{
open Scish_parse
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

let id = ['A'-'Z''a'-'z''_']['a'-'z''A'-'Z''0'-'9''_']*

(* rules section *)
rule lexer = parse
| eol { incr_lineno lexbuf; lexer lexbuf }
| ws+ { lexer lexbuf }
| digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) }
| '(' { LPAREN }
| ')' { RPAREN }
| '=' { EQ }
| '<' { LT }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIV }
| "cons" { CONS }
| "fst" { FST }
| "snd" { SND }
| "lambda" { LAMBDA }
| "if" { IF }
| "let" { LET }
| "letrec" { LETREC }
| id { ID(Lexing.lexeme lexbuf) }
| "/*" { comment lexbuf }
| eof { EOF }
and comment = parse
| eol { incr_lineno lexbuf; comment lexbuf }
| "*/" { lexer lexbuf }
| _ { comment lexbuf }

