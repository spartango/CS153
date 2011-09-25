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

%right ASSIGN
%left OR
%left AND
%right EQ NEQ GTE GT LTE LT
%left PLUS MINUS
%left TIMES DIV
%right NOT


/* Here's where the real grammar starts -- you'll need to add 
 * more rules here... Do not remove the 2%'s!! */
 %%

program:
  stmt EOF { $1 }

stmt :
    | exp SEMI stmt  { (Ast.Seq((Ast.Exp($1), (rhs 1)), $3), (rhs 2)) }
    | RETURN exp SEMI       { (Ast.Return($2), (rhs 1)) }
    | LCURLY stmt RCURLY    { ($2) }
    | /* empty */      { (Ast.skip, 0) } 

exp:
    /* Terminals */
    | INT                   {  (Ast.Int($1), (rhs 1)) }
    /* Assignment */
    | ID ASSIGN exp         { (Ast.Assign($1,$3), (rhs 2)) }
    | ID                    { (Ast.Var($1), (rhs 1)) }
    | LPAREN exp RPAREN     {  ( $2 ) }
    /* Binary operators */
    | exp PLUS exp          { (Ast.Binop($1,Plus,$3), (rhs 2)) }
    | exp MINUS exp         { (Ast.Binop($1,Minus,$3), (rhs 2)) }
    | exp TIMES exp         { (Ast.Binop($1,Times,$3), (rhs 2)) }
    | exp DIV exp           { (Ast.Binop($1,Div,$3), (rhs 2)) }
    | exp EQ exp            { (Ast.Binop($1,Eq,$3), (rhs 2)) }
    | exp NEQ exp           { (Ast.Binop($1,Neq,$3), (rhs 2)) }
    | exp GTE exp           { (Ast.Binop($1,Gte,$3), (rhs 2)) }
    | exp GT exp            { (Ast.Binop($1,Gt,$3), (rhs 2)) }
    | exp LTE exp           { (Ast.Binop($1,Lt,$3), (rhs 2)) }
    | exp LT exp            { (Ast.Binop($1,Lte,$3), (rhs 2)) }
    | exp AND exp           { (Ast.And($1,$3), (rhs 2)) }
    | exp OR exp            { (Ast.Or($1,$3), (rhs 2)) }
    /* Negation */
    | MINUS exp             { (Ast.Binop((Ast.Int(-1), 0),Times,$2), (rhs 1)) }
    | NOT exp               { (Ast.Not($2), (rhs 1)) }



/*

 
*/
