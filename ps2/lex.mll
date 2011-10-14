(* header section *)
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

let id = ['A'-'Z''a'-'z''_']['a'-'z''A'-'Z''0'-'9''_']*

(* rules section *)

rule lexer = parse
| eol { incr_lineno lexbuf; lexer lexbuf } 
| ws+ { lexer lexbuf }
| ';' { SEMI }
| '{' { LBRACE }
| '}' { RBRACE }
| '(' { LPAREN } 
| ')' { RPAREN }
| "==" { EQEQ }
| "=" { EQ }
| "!=" { NEQ }
| "<=" { LTE }
| ">=" { GTE }
| '<' { LT }
| '>' { GT }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIV }
| "&&" { AND }
| "||" { OR }
| '!' { BANG }
| "return" { RETURN }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "for" { FOR }
| id { ID(Lexing.lexeme lexbuf) }
| digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) } 
| "/*" { comment lexbuf } (* comment start *)
| eof { EOF }
and comment = parse 
| eol { incr_lineno lexbuf; comment lexbuf }
| "*/" { lexer lexbuf } (* comment end *)
| _ { comment lexbuf }

(* trailer section *)
