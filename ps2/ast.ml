(* The abstract syntax for our little subset of Fortran *)
type var = string
type pos = int      (* position is line number in source file *)

type binop = 
  Plus | Minus | Times | Div          (* +, -, *, /           *)
| Eq | Neq | Lt | Lte | Gt | Gte      (* ==, !=, <, <=, >, >= *)

type rexp = 
  Int of int
| Var of var
| Binop of exp * binop * exp
| Not of exp                          (* !x *)
| And of exp * exp                    (* x < y && y < z *)
| Or of exp * exp                     (* x < y || x < z *)
| Assign of var * exp                 (* x = y+42 *)

(* every expression comes with its position *)
and exp = rexp * pos

type rstmt = 
  Exp of exp                          (* x = 3+4; *)
| Seq of stmt * stmt                  (* x = 2*9; y = 42; *)
| If of exp * stmt * stmt             (* if (x == y) x = 42 else y = 43 *)
| While of exp * stmt                 (* while (x < y) x = x + 1; *)
| For of exp * exp * exp * stmt       (* for (x=0; x<y; x=x+1) y=y*42; *)
| Return of exp                       (* return e; *)

(* every statement comes with its position *)
and stmt = rstmt * pos

let skip : rstmt = Exp(Int 0,0)          (* simulate a skip statement *)

type program = stmt
