%{
open Ast
open Lexing
(* use this to get the line number for the n'th token *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n") 
%}

%start program

/* nonterminals */
%type <Ast.program> program
%type <Ast.func> func
%type <Ast.var list> idlist
%type <Ast.var list> formals
%type <Ast.stmt> stmt
%type <Ast.stmt> stmtlist
%type <Ast.exp> yexp
%type <Ast.exp list> explist
%type <Ast.exp> orexp
%type <Ast.exp> andexp
%type <Ast.exp> equalexp
%type <Ast.exp> relnexp
%type <Ast.exp> addexp
%type <Ast.exp> mulexp
%type <Ast.exp> unaryexp
%type <Ast.exp> atomicexp
%type <Ast.exp option> expopt

/* terminals */
%token SEMI LPAREN RPAREN LBRACE RBRACE
%token EQEQ NEQ LTE GTE LT GT EQ BANG 
%token PLUS MINUS TIMES DIV AND OR
%token RETURN IF ELSE WHILE FOR
%token LET COMMA
%token <int> INT 
%token <string> ID 
%token EOF

/* Start grammer rules*/
%%

program:
  func { [$1] }
| func program { $1::$2 }

func :
  ID formals LBRACE stmtlist RBRACE { Fn{name=$1;args=$2;body=$4;pos=rhs 1} }

formals :
  LPAREN RPAREN { [] }
| LPAREN idlist RPAREN { $2 }

idlist :
  ID { [$1] }
| ID COMMA idlist { $1::$3 }

stmt : 
  SEMI { (skip, rhs 1) }
| yexp SEMI { (Exp $1, rhs 1) }
| RETURN yexp SEMI { (Return $2, rhs 1) }
| LBRACE stmtlist RBRACE { $2 }
| IF LPAREN yexp RPAREN stmt ELSE stmt { (If($3,$5,$7), rhs 1) } 
| IF LPAREN yexp RPAREN stmt { (If($3,$5,(skip, rhs 5)), rhs 1) } 
| WHILE LPAREN yexp RPAREN stmt { (While($3,$5), rhs 1) }
| FOR LPAREN expopt SEMI expopt SEMI expopt RPAREN stmt {
      let e1 = match $3 with None -> (Int(0), rhs 3) | Some e -> e in
      let e2 = match $5 with None -> (Int(1), rhs 5) | Some e -> e in
      let e3 = match $7 with None -> (Int(0), rhs 7) | Some e -> e in
      (For(e1,e2,e3,$9), rhs 1)
    }
| LET ID EQ yexp SEMI stmt { (Let($2,$4,$6), rhs 1) }

stmtlist :
  stmt { $1 }
| stmt stmtlist { (Seq($1,$2), rhs 1) }

expopt : 
  { None }
  | yexp { Some $1 }

yexp:
  orexp { $1 }
| ID EQ yexp { (Assign($1,$3), rhs 1) }

explist :
  yexp { [$1] }
| yexp COMMA explist { $1::$3 }

orexp :
  andexp { $1 }
| orexp OR andexp { (Or($1,$3), rhs 1) }

andexp :
  equalexp { $1 }
| andexp AND equalexp { (And($1,$3), rhs 1) }

equalexp :
  relnexp { $1 }
| equalexp EQEQ relnexp { (Binop($1,Eq,$3), rhs 1) }
| equalexp NEQ relnexp { (Binop($1,Neq,$3), rhs 1) }

relnexp :
  addexp { $1 }
| relnexp LT addexp { (Binop($1,Lt,$3), rhs 1) }
| relnexp GT addexp { (Binop($1,Gt,$3), rhs 1) }
| relnexp LTE addexp { (Binop($1,Lte,$3), rhs 1) }
| relnexp GTE addexp { (Binop($1,Gte,$3), rhs 1) }

addexp :
  mulexp { $1 }
| addexp PLUS mulexp { (Binop($1,Plus,$3), rhs 1) }
| addexp MINUS mulexp { (Binop($1,Minus,$3), rhs 1) }

mulexp :
  unaryexp { $1 }
| mulexp TIMES unaryexp { (Binop($1,Times,$3), rhs 1) }
| mulexp DIV unaryexp { (Binop($1,Div,$3), rhs 1) }

unaryexp :
  atomicexp { $1 }
| PLUS unaryexp { $2 }
| MINUS unaryexp { (Binop((Int 0,rhs 1),Minus,$2), rhs 1) }
| BANG unaryexp { (Not $2, rhs 1) }

atomicexp :
  INT { (Int $1, rhs 1) }
| ID { (Var $1, rhs 1) }
| ID LPAREN RPAREN { (Call($1,[]), rhs 1) }
| ID LPAREN explist RPAREN { (Call($1,$3), rhs 1) }
| LPAREN yexp RPAREN { $2 }
