(* An interpreter for Fish programs *)
open Ast

let bool2int (b:bool):int = if b then 1 else 0

exception BadProgram
let error s = (print_string ("Error: "^ s); raise BadProgram)

type value = Int_v of int ref | Fun_v of func
type env = var -> value
let empty_env (x:var) : value = error ("unbound variable "^x)
let lookup (x:var) (env:env) : value = env x
let insert (x:var) (v:value) (env:env) = fun y -> if y = x then v else env y

let insert_int x i env = insert x (Int_v(ref i)) env
let insert_fn env f = 
    let Fn(f2) = f in
    insert f2.name (Fun_v f) env
let lookup_int (x:var) (env:env) : int ref = 
    match lookup x env with
      Int_v r -> r
    | _ -> error("variable "^x^" is a function, not an int")
let lookup_fn x env = 
    match lookup x env with
      Fun_v f -> f
    | _ -> error("variable "^x^" is an int, not a function")

let rec zip_args = function
    (x::xs,i::is,env) -> zip_args(xs,is,insert_int x i env)
  | ([],[],env) -> env
  | (_,[],_) -> error("too few args")
  | ([],_,_) -> error("too many args")
  
(* We signal "returning" a value
 * for the program by throwing the exception Done.  *)
exception Done of int

(* Evaluate a Cish expression returning an integer *)
let rec eval_exp ((e:rexp),(pos:int)) (env:env) : int = 
    match e with
      Int i -> i
    | Var x -> !(lookup_int x env)
    | Binop(e1,b,e2) ->
      let (i1,i2) = (eval_exp e1 env, eval_exp e2 env) in (
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
    | Not e1 -> bool2int ((eval_exp e1 env) = 0) 
    | And(e1,e2) -> if (eval_exp e1 env) <> 0 then eval_exp e2 env else 0
    | Or(e1,e2) -> if (eval_exp e1 env) <> 0 then 1 else eval_exp e2 env
    | Assign(x,e1) -> 
        let r = lookup_int x env in
        let i = eval_exp e1 env in
        let _ =  r := i in 
        i
    | Call(f,es) -> 
        let Fn(f2) = lookup_fn f env in
        let new_env = zip_args(f2.args,List.map (fun e -> eval_exp e env) es,env) in
        try (eval_stmt f2.body new_env; 0) with Done i -> i

and eval_stmt ((s:rstmt),(pos:int)) (env:env) : unit = 
    match s with 
      Exp e -> let _ = eval_exp e env in ()
    | Seq(s1,s2) -> let _ = eval_stmt s1 env in eval_stmt s2 env
    | If(e,s1,s2) -> 
        if (eval_exp e env) <> 0 then eval_stmt s1 env else eval_stmt s2 env
    | While(e,s1) -> 
        eval_stmt (If(e,(Seq(s1,(s,pos)),pos),(skip,pos)),pos) env
    | For(e1,e2,e3,s1) ->
        let _ = eval_exp e1 env in
        eval_stmt (While(e2, (Seq(s1,(Exp e3,pos)),pos)), pos) env
    | Return e -> let i = eval_exp e env in raise (Done i)
    | Let(x,e,s) -> 
        let i = eval_exp e env in
        let env = insert_int x i env in
        eval_stmt s env

let eval (p:program):int = 
  let env = List.fold_left insert_fn empty_env p in
  let Fn(f) = lookup_fn "main" env in
  try (eval_stmt f.body env; 0) with Done i -> i
