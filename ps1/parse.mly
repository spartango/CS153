/* Parser for Fish --- TODO */

%{
open Ast
open Lexing
(* use this to get the line number for the n'th token  
 * in the rule *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum

let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n") 
%}

/* Tells us which non-terminal to start the grammar with. */
%start program

/* This specifies the non-terminals of the grammar and specifies the
 * types of the values they build. Don't forget to add any new non-
 * terminals here.
 */
%type <Ast.program> program
%type <Ast.stmt> stmt
%type <Ast.exp> exp

/* The %token directive gives a definition of all of the terminals
 * (i.e., tokens) in the grammar. This will be used to generate the
 * tokens definition used by the lexer. So this is effectively the
 * interface between the lexer and the parser --- the lexer must
 * build values using this datatype constructor to pass to the parser.
 * You will need to augment this with your own tokens...
 */

/* Tokens - names capitalized with lex/yacc conventions */
%token <string> ID
%token <int> INT
%token EOF SEMI RPAREN LPAREN RCURLY LCURLY
%token EQ NEQ GTE GT LTE LT OR AND ASSIGN PLUS MINUS TIMES DIV 
%token FOR IF ELSE WHILE RETURN
%token NOT


/* Here's where the real grammar starts -- you'll need to add 
 * more rules here... Do not remove the 2%'s!! */
 %%

program:
  stmt EOF { $1 }

stmt :
    | exp SEMI         {  (Ast.Exp($1), (rhs 1)) }
    | RETURN exp SEMI  { (Ast.Return($2), (rhs 1)) }
    | /* empty */      { (Ast.skip, 0) } 

exp:
    | INT      {  (Ast.Int($1), (rhs 1)) }
    | ID       {  (Ast.Var($1), (rhs 1)) }

