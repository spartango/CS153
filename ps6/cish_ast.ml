(* The abstract syntax for our little subset of C -- includes rudimentary 
 * printing support now.
 *)
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
| Call of exp * (exp list)            (* f(x,y,z) *)
| Load of exp                         (* *(x+3) *)
| Store of exp * exp                  (* *(x+3) = e *)
| Malloc of exp                       (* malloc(i) *)

(* every expression comes with its position *)
and exp = rexp * pos

type rstmt = 
  Exp of exp                          (* x = 3+4; *)
| Seq of stmt * stmt                  (* x = 2*9; y = 42; *)
| If of exp * stmt * stmt             (* if (x == y) x = 42 else y = 43 *)
| While of exp * stmt                 (* while (x < y) x = x + 1; *)
| For of exp * exp * exp * stmt       (* for (x=0; x<y; x=x+1) y=y*42; *)
| Return of exp                       (* return e; *)
| Let of var * exp * stmt             (* let x=3; in x=x+1; *)

(* every statement comes with its position *)
and stmt = rstmt * pos

type funcsig =  { name : var; args : var list; body : stmt; pos : pos } 
type func = Fn of funcsig 

let skip : rstmt = Exp(Int 0,0)          (* simulate a skip statement *)

type program = func list

(***************************************************************)
(* functions for printing out Cish syntax                      *)
(***************************************************************)

(* binops to strings *)
let binop2s (b:binop):string = 
    match b with
      Plus -> "+"
    | Times -> "*"
    | Minus -> "-"
    | Div -> "/"
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Lte -> "<="
    | Gt -> ">"
    | Gte -> ">="

(* expressions 2 strings *)
(* avoid parentheses by tracking precedence *)
let rec e2s (p:int) ((e,_):exp) : string = 
    let prec e = 
        match e with
          Int i -> 100
        | Var x -> 100
        | Binop(_,Plus,_) -> 50
        | Binop(_,Minus,_) -> 50
        | Binop(_,Times,_) -> 75
        | Binop(_,Div,_) -> 75
        | Binop(_,Eq,_) -> 25
        | Binop(_,Neq,_) -> 25
        | Binop(_,Lt,_) -> 30
        | Binop(_,Lte,_) -> 30
        | Binop(_,Gt,_) -> 30
        | Binop(_,Gte,_) -> 30
        | Not e -> 35
        | Or _ -> 15
        | And _ -> 20
        | Assign _ -> 5
        | Call _ -> 100
        | Load _ -> 80
        | Store _ -> 5
        | Malloc _ -> 100 in
    let myprec = prec e in
    let (start,stop) = if myprec >= p then ("","") else ("(",")") in
    start ^ (match e with
      Int i -> string_of_int i
    | Var x -> x
    | Binop(e1,b,e2) -> (e2s myprec e1) ^ (binop2s b) ^ (e2s myprec e2)
    | Not e -> "!" ^ (e2s myprec e)
    | And(e1,e2) -> (e2s myprec e1) ^ " && " ^ (e2s myprec e2)
    | Or(e1,e2) -> (e2s myprec e1) ^ " || " ^ (e2s myprec e2)
    | Assign(x,e) -> x ^ " = " ^ (e2s myprec e)
    | Call(e,es) -> (e2s myprec e) ^ "(" ^ (es2s es) ^ ")"
    | Load e -> "*" ^ (e2s myprec e)
    | Store(e1,e2) -> "*"^(e2s 80 e1)^" = "^(e2s myprec e2)
    | Malloc e -> "malloc("^(e2s myprec e)^")") ^ stop
and es2s (es:exp list) : string = 
    match es with
      [] -> ""
    | e::[] -> e2s 0 e
    | e::es -> (e2s 0 e)^","^(es2s es)

(* expression to string -- default precedence of zero *)
let exp2string e = e2s 0 e

(* used to get proper indentation *)
let rec tab = function
    0 -> ""
  | n -> " "^(tab (n-1))

(* convert a statement to string -- i tracks the current nesting depth *)
let rec s2s i (s,_) = 
    match s with
      Exp e -> (tab i) ^ (exp2string e) ^ ";\n"
    | Seq (s1,s2) -> (s2s i s1)^(s2s i s2)
    | Return e -> (tab i) ^ "return "^(exp2string e)^";\n"
    | If(e,s1,s2) -> 
      (tab i)^"if ("^(exp2string e)^") {\n"^
      (s2s (i+2) s1)^(tab i)^"} else {\n"^
      (s2s (i+2) s2)^(tab i)^"}\n"
    | While(e,s) -> 
      (tab i)^"while ("^(exp2string e)^") {\n"^
      (s2s (i+2) s)^(tab i)^"}\n"
    | For(e1,e2,e3,s) -> 
      (tab i)^"for ("^(exp2string e1)^","^(exp2string e2)^","^
                       (exp2string e3)^") {\n"^(s2s (i+2) s)^(tab i)^"}\n"
    | Let(x,e,s) -> 
      (tab i)^"let "^x^" = "^(exp2string e)^"; {\n"^(s2s (i+2) s)^(tab i)^"}\n"
(* convert a statement to string with starting nesting depth of 0 *)
let stmt2string s = s2s 0 s

(* convert a function to a string *)
let fn2string f = 
    let Fn (f') = f in
    f'.name ^ "(" ^ (String.concat "," f'.args) ^ ") {\n" ^
    (s2s 3 f'.body) ^ "}\n"

(* convert a program to a string *)
let prog2string fs = String.concat "" (List.map fn2string fs)

