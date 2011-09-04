%{
open Mips_ast
open Lexing

let rh n = 
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n")
%}

%start program

%type <Mips_ast.program> program
%type <Mips_ast.program> instlst
%type <Mips_ast.inst> inst

%token ADD BEQ JR JAL LI LW SW LUI ORI COMMA LPAREN RPAREN 
%token <int32> INT
%token <string> ID
%token <string> REG
%token EOF

%%

program :
  instlst EOF { $1 }

instlst : 
  inst { [$1] }
| inst instlst { $1::$2 }

inst :
  ADD REG COMMA REG COMMA REG { Add(str2reg $2,str2reg $4,str2reg $6) }
| BEQ REG COMMA REG COMMA INT { Beq(str2reg $2,str2reg $4,$6) }
| JR REG { Jr(str2reg $2) }
| JAL INT { Jal($2) }
| LI REG COMMA INT { Li(str2reg $2,$4) }
| LUI REG COMMA INT { Lui(str2reg $2,$4) }
| ORI REG COMMA REG COMMA INT { Ori(str2reg $2,str2reg $4,$6) }
| LW REG COMMA INT LPAREN REG RPAREN { Lw(str2reg $2,str2reg $6,$4) }
| SW REG COMMA INT LPAREN REG RPAREN { Sw(str2reg $2,str2reg $6,$4) }

