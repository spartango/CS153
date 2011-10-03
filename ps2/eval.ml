(* An interpreter for Fish programs *)
open Ast

(* We're going to represent memory for Fish programs using a 
 * Hashtable mapping variables to integer references. *)

let vartable = Hashtbl.create 33
  
(* Create a string hash table with initial size 33 *)
(* val vartable : int ref StringHash.hashTable = StringHash.new 33 *)

(* Lookup a variable's ref in the hash table.  If it doesn't have
 * an entry, just insert a new one in the hashtable. *)
let lookup (x:var) = 
  try (Hashtbl.find vartable x) 
  with Not_found ->
    let r = ref 0 in
    Hashtbl.add vartable x r;
    r

(* Set a variables value in the hash table -- again, if the variable
 * wasn't there previously, then we'll just insert a fresh ref. *)
let set (x:var) (i:int) : int = 
  let r = lookup x in
    r := i; i

let bool2int (b:bool):int = if b then 1 else 0

(* Evaluate a Fish expression returning an integer *)
let rec eval_exp ((e:rexp),(pos:int)) : int = 
  match e with
    Int i -> i
  | Var x -> !(lookup x)
  | Binop(e1,b,e2) ->
      let (i1,i2) = (eval_exp e1, eval_exp e2) in (
        match b with
          Plus -> i1 + i2
        | Minus -> i1 - i2
        | Times -> i1 * i2
        | Div -> i1 / i2
        | Eq -> bool2int (i1 = i2) 
        | Neq -> bool2int (i1 <> i2) 
        | Lt -> bool2int (i1 < i2)
        | Lte -> bool2int (i1 <= i2)
        | Gt -> bool2int (i1 > i2)
        | Gte -> bool2int (i1 >= i2)
      ) 
  | Not e1 -> bool2int (eval_exp e1 = 0) 
  | And(e1,e2) -> 
      if (eval_exp e1) <> 0 then eval_exp e2 else 0
  | Or(e1,e2) ->
      if (eval_exp e1) <> 0 then 1 else eval_exp e2
  | Assign(x,e1) -> set x (eval_exp e1)

(* Evaluate a fish statement.  We signal "returning" a value
 * for the program by throwing the exception Done.  *)
exception Done of int

let rec eval_stmt ((s:rstmt),(pos:int)) : unit = 
  match s with 
    Exp e -> let _ = eval_exp e in () 
  | Seq(s1,s2) -> (eval_stmt s1; eval_stmt s2)
  | If(e,s1,s2) -> 
      if (eval_exp e) <> 0 then eval_stmt s1 else eval_stmt s2
  | While(e,s1) -> eval_stmt (If(e,(Seq(s1,(s,pos)),pos),(skip,pos)),pos)
  | For(e1,e2,e3,s1) -> (
      let _ = eval_exp e1 in
      eval_stmt (While(e2, (Seq(s1,(Exp e3,pos)),pos)), pos)
    )
  | Return e -> raise (Done (eval_exp e))

exception BadProgram

let eval (p:program):int = 
  try
    (eval_stmt p; 
     print_string "Error -- program terminated without returning!\n";
     raise BadProgram
    ) with Done i -> i
