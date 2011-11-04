%{
open Scish_ast
open Lexing
let parse_error s =
  let pos = Parsing.symbol_end_pos() in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n")
%}

%start program

/* nonterminals */
%type <Scish_ast.exp> program
%type <Scish_ast.exp> exp
%type <Scish_ast.exp list> exps
%type <Scish_ast.primop> primop

/* terminals */
%token IF LPAREN RPAREN LAMBDA EOF
%token LT EQ LET LETREC CONS FST SND
%token PLUS MINUS TIMES DIV
%token <int> INT
%token <string> ID

/* Start grammer rules */
%%

program : 
  exp EOF { $1 }

exp :
  INT { Int($1) }
| ID { Var($1) }
| LPAREN primop exps RPAREN { PrimApp($2,$3) }
| LPAREN LAMBDA LPAREN ID RPAREN exp RPAREN { Lambda($4,$6) }
| LPAREN exp exp RPAREN { App($2,$3) }
| LPAREN IF exp exp exp RPAREN { If($3,$4,$5) }
| LPAREN LET LPAREN ID exp RPAREN exp RPAREN { sLet $4 $5 $7 }

exps :
  { [] }
| exp exps { $1::$2 }

primop :
  PLUS { Plus }
| MINUS { Minus }
| TIMES { Times }
| DIV { Div }
| EQ { Eq }
| LT { Lt }
| CONS { Cons }
| FST { Fst }
| SND { Snd }
