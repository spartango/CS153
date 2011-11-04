%{
open Mlish_ast
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

/* nonterminal */
%type <Mlish_ast.exp> program
%type <Mlish_ast.exp> exp
%type <Mlish_ast.exp> app_exp
%type <Mlish_ast.exp> aexp
%type <Mlish_ast.exp> bexp
%type <Mlish_ast.exp> cexp
%type <Mlish_ast.exp> dexp
%type <(Mlish_ast.exp * int) list> exp_list
%type <Mlish_ast.var list> id_list

/* terminals */
%token TRUE FALSE HD TL FST SND ISNIL
%token PLUS MINUS TIMES DIV LPAREN RPAREN
%token IF THEN ELSE EQUALS LT COMMA SEMI
%token TILDE NIL LBRACKET RBRACKET CONS
%token DARROW LET IN END FN VAL EOF
%token <int> INT
%token <string> ID

/* define precedences */
%right CONS
%right DARROW
%left ELSE
%left LT

%%

program : 
  exp { $1 }

exp : 
  cexp { $1 }
| FN id_list DARROW exp { List.fold_right (fun x y -> (Fn(x,y), rhs 1)) $2 $4 }
| IF exp THEN exp ELSE exp { (If($2,$4,$6), rhs 1) }

cexp :
  dexp { $1 }
| cexp LT dexp { (PrimApp(Lt,[$1;$3]), rhs 1) }
| cexp EQUALS dexp { (PrimApp(Eq,[$1;$3]), rhs 1) }

dexp :
  app_exp { $1 }
| app_exp CONS dexp { (PrimApp(Cons,[$1;$3]), rhs 1) }

app_exp :
  aexp { $1 }
| app_exp PLUS aexp { (PrimApp(Plus,[$1;$3]), rhs 1) }
| app_exp MINUS aexp { (PrimApp(Minus,[$1;$3]), rhs 1) }
| TILDE MINUS aexp { (PrimApp(Minus,[(PrimApp(Int 0,[]), rhs 1);$3]), rhs 1) }

aexp :
  bexp { $1 }
| aexp TIMES bexp { (PrimApp(Times,[$1;$3]), rhs 1) }
| aexp DIV bexp { (PrimApp(Div,[$1;$3]), rhs 1) }

bexp1 : 
  INT { (PrimApp(Int($1),[]), rhs 1) }
| TRUE { (PrimApp(Bool(true),[]), rhs 1) }
| FALSE { (PrimApp(Bool(false),[]), rhs 1) }
| NIL { (PrimApp(Nil,[]), rhs 1) }
| HD { (Fn ("x", (PrimApp(Hd,[(Var "x",0)]),0)), rhs 1) }
| TL { (Fn ("x", (PrimApp(Tl,[(Var "x",0)]),0)), rhs 1) }
| FST { (Fn ("x", (PrimApp(Fst,[(Var "x",0)]),0)), rhs 1) }
| SND { (Fn ("x", (PrimApp(Snd,[(Var "x",0)]),0)), rhs 1) }
| ISNIL { (Fn ("x", (PrimApp(IsNil,[(Var "x",0)]),0)), rhs 1) }
| ID { (Var($1), rhs 1) }
| LET ID EQUALS exp IN exp { (Let($2,$4,$6), rhs 1) }
| LET ID id_list EQUALS exp IN exp 
    { let fun_exp = List.fold_right (fun x y -> (Fn(x,y), rhs 3)) $3 $5 in
      (Let($2,fun_exp,$7), rhs 1) }
| LPAREN RPAREN { (PrimApp(Unit,[]), rhs 1) }
| LPAREN exp RPAREN { $2 }
| LPAREN exp COMMA exp RPAREN { (PrimApp(Pair,[$2;$4]), rhs 1) }
| LBRACKET RBRACKET { (PrimApp(Nil,[]), rhs 1) }
| LBRACKET exp_list RBRACKET 
    { List.fold_right (fun (e1,p) e2 -> 
        (PrimApp(Cons,[e1;e2]),p)) $2 (PrimApp(Nil,[]), rhs 3) }

bexp :
  INT { (PrimApp(Int($1),[]), rhs 1) }
| TRUE { (PrimApp(Bool(true),[]), rhs 1) }
| FALSE { (PrimApp(Bool(false),[]), rhs 1) }
| NIL { (PrimApp(Nil,[]), rhs 1) }
| HD { (Fn ("x", (PrimApp(Hd,[(Var "x",0)]),0)), rhs 1) }
| TL { (Fn ("x", (PrimApp(Tl,[(Var "x",0)]),0)), rhs 1) }
| FST { (Fn ("x", (PrimApp(Fst,[(Var "x",0)]),0)), rhs 1) }
| SND { (Fn ("x", (PrimApp(Snd,[(Var "x",0)]),0)), rhs 1) }
| ISNIL { (Fn ("x", (PrimApp(IsNil,[(Var "x",0)]),0)), rhs 1) }
| ID { (Var($1), rhs 1) }
| bexp bexp1 { (App($1,$2), rhs 1) }
| LET ID EQUALS exp IN exp { (Let($2,$4,$6), rhs 1) }
| LET ID id_list EQUALS exp IN exp 
    { let fun_exp = List.fold_right (fun x y -> (Fn(x,y), rhs 3)) $3 $5 in
      (Let($2,fun_exp,$7), rhs 1) }
| LPAREN RPAREN { (PrimApp(Unit,[]), rhs 1) }
| LPAREN exp RPAREN { $2 }
| LPAREN exp COMMA exp RPAREN { (PrimApp(Pair,[$2;$4]), rhs 1) }
| LBRACKET RBRACKET { (PrimApp(Nil,[]), rhs 1) }
| LBRACKET exp_list RBRACKET 
    { List.fold_right (fun (e1,p) e2 -> 
        (PrimApp(Cons,[e1;e2]),p)) $2 (PrimApp(Nil,[]), rhs 3) }

exp_list :
  exp { [($1, rhs 1)] }
| exp SEMI exp_list { ($1, rhs 1)::$3 }

id_list :
  ID { [$1] }
| ID id_list { $1::$2 }
