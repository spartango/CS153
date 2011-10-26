(* Abstract syntax and pretty-printer for the Scheme-ish language Scish *)
type var = string

type primop = 
  Plus   (* add two ints *)
| Minus  (* subtract two ints *)
| Times  (* multiply two ints *)
| Div    (* divide two ints *)
| Cons   (* create a pair *)
| Fst    (* fetch the 1st component of a pair *)
| Snd    (* fetch the 2nd component of a pair *)
| Eq     (* compare two ints for equality *)
| Lt     (* compare two ints for inequality *)

type exp = 
  Int of int                    (* integer constants *)
| Var of var                    (* read value in variable *)
| PrimApp of primop * exp list  (* apply primitive operation *)
| Lambda of var * exp           (* an anonymous function *)
| App of exp * exp              (* call a function *)
| If of exp * exp * exp         (* if e1 != 0 then e2 else e3 *)

(* some derived forms *)
let sLet (x:var) (e1:exp) (e2:exp) : exp = App(Lambda(x,e2),e1)          

(**********************************************************)
(* Pretty printing                                        *)
(**********************************************************)
let primop2string p = 
  match p with
    Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Cons -> "cons"
  | Fst -> "fst"
  | Snd -> "snd"
  | Eq -> "=" 
  | Lt -> "<"

let rec exp2string (e:exp):string = 
  match e with
    Int i -> string_of_int i
  | PrimApp(p,es) -> 
      "(" ^ (primop2string p) ^ " " ^ (exps2string es) ^")"
  | Var x -> x
  | Lambda(x,e) -> "(lambda ("^x^") ("^(exp2string e)^"))"
  | App(e1,e2) -> "(" ^ (exps2string [e1;e2]) ^ ")"
  | If(e1,e2,e3) -> "(if "^(exps2string[e1;e2;e3]) ^ ")"

and exps2string (es:exp list):string = 
  String.concat " " (List.map exp2string es)

