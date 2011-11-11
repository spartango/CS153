(* An interpreter for Cish programs -- includes some rudimentary debugging
 * support which you should feel free to augment...
 *)
open Cish_ast

(* Cish values are either integers or functions *)
type value = Int_v of int | Fun_v of func

(* for debugging *)
let val2string = function
    (Int_v i) -> string_of_int i
  | (Fun_v (Fn f)) -> f.name

(* set this flag to true to turn on diagnostics when evaluating *)
let debug_flag = ref false

let debug f x = if (!debug_flag) then f x else ()

let bool2int (b:bool):int = if b then 1 else 0

exception BadProgram
let error s = (print_string ("Error: "^s^"\n"); raise BadProgram)

(* The state of the interpreter includes bindings for variables 
 * (varenv), a memory to hold heap-allocated values (memory),
 * and an allocation pointer, pointing to the next available 
 * location in the memory.  The memory and allocation pointer
 * record *words*, not bytes. *)
type env = { varenv : var -> value ref;
             memory : value array;
             allocptr : int ref 
           }

(* Create the initial environment -- adjust memory size as needed
 * below. *)
let memory_size = 1000   (* default is 1000 words *)
let empty_varenv(x:var):value ref = error ("unbound variable "^x)
let empty_env():env = {varenv = empty_varenv;
                       memory = Array.make memory_size (Int_v 0xf00);
                       allocptr = ref 0 }

(* Lookup variables in the environment *)
let lookup_ref (x:var) (env:env):value ref = (env.varenv) x
let lookup (x:var) (env:env):value = !(lookup_ref x env)
let lookup_int (x:var) (env:env) : int = 
    match lookup x env with
      Int_v i -> i
    | _ -> error("variable "^x^" is a function, not an int")
let lookup_fn (x:var) (env:env) = 
    match lookup x env with
      Fun_v f -> f
    | _ -> error("variable "^x^" is an int, not a function")

(* Insert a new variable into the environment *)
let insert (x:var) (v:value) (env:env) : env = 
    let r = ref v in
    let varenv = fun y -> if y = x then r else (env.varenv) y in
    { varenv = varenv; memory = env.memory; allocptr = env.allocptr }
    
let insert_int x i env = insert x (Int_v(i)) env
let insert_fn env f = 
    let Fn(f') = f in
    insert f'.name (Fun_v f) env

(* Insert bindings for formal to actual arguments in the environment *)
let rec zip_args = function
    (x::xs,v::vs,env) -> zip_args(xs,vs,insert x v env)
  | ([],[],env) -> env
  | (_,[],_) -> error("too few args")
  | ([],_,_) -> error("too many args")
  
(* We signal "returning" a value by throwing the exception Done.  *)
exception Done of value

(* Utility function for dumping out contents of memory *)
let dump_mem (env:env) = 
    let mem = env.memory in
    let ap = !(env.allocptr) in
    let rec loop n = 
      if (n < ap) then 
        (print_string ("["^(string_of_int (n*4))^": "^
                      (val2string(Array.get mem n))^"]");
        loop(n+1))
      else (print_string "\n") in
    loop 0

let dump_mem = debug dump_mem

(* Evaluate a Cish expression and check that the result is an integer *)
let rec eval_int (e:exp) (env:env) : int = 
    match eval_exp e env with
      Int_v i -> i
    | _ -> error("expression "^(exp2string e)^" is a function, not an int")
(* Evaluate a Cish expression and check that the result is a function *)
and eval_fun (e:exp) (env:env) = 
    match eval_exp e env with
      Fun_v f -> f
    | _ -> error("expression "^(exp2string e)^" is an int, not a function")
(* Evaluate a Cish expression and return the corresponding value *)
and eval_exp (e:exp) (env:env) : value = 
    let (r,pos) = e in
    let res =  
      match r with
        Int i -> Int_v i
      | Var x -> lookup x env
      | Binop(e1,b,e2) ->
          let (i1,i2) = (eval_int e1 env, eval_int e2 env) in
          Int_v(match b with
                  Plus -> i1 + i2
                | Minus -> i1 - i2
                | Times -> i1 * i2
                | Div -> i1 / i2
                | Eq -> bool2int (i1 = i2) 
                | Neq -> bool2int (i1 <> i2) 
                | Lt -> bool2int (i1 < i2)
                | Lte -> bool2int (i1 <= i2)
                | Gt -> bool2int (i1 > i2)
                | Gte -> bool2int (i1 >= i2))      
      | Not e1 -> Int_v(bool2int ((eval_int e1 env) = 0))
      | And(e1,e2) -> 
          if (eval_int e1 env) <> 0 then eval_exp e2 env else Int_v 0
      | Or(e1,e2) ->
          if (eval_int e1 env) <> 0 then Int_v 1 else eval_exp e2 env
      | Assign(x,e1) -> 
          let r = lookup_ref x env in
          let v = eval_exp e1 env in
          let _ = r := v in
          v
      | Call(e,es) -> 
          let Fn (f) = eval_fun e env in
          let new_env = zip_args(f.args,List.map (fun e -> eval_exp e env) es,env) in
          (try (eval_stmt f.body new_env; Int_v 0) with Done v -> v)
      | Load(e1) -> Array.get env.memory ((eval_int e1 env) / 4) 
      | Store(e1,e2) -> 
          let i = eval_int e1 env in
          let v = eval_exp e2 env in
          let _ = Array.set env.memory (i / 4) v in
          v
      | Malloc(e) -> 
          (* malloc(i) is supposed to return a pointer to space that
           * can hold i/4 words.  (The paramter i is the number of 
           * bytes to allocate.)  We check that there's enough space
           * in the memory, and then assuming there is, return the 
           * value of the allocation pointer, after incrementing it
           * by i/4. *)
          let i = (eval_int e env) / 4 in  (* 4 bytes per value *)
          let len = Array.length(env.memory) in
          let ap = (env.allocptr) in
          let p = !ap in
          if (p + i >= len) then error("out of heap memory")
          else (ap := p+i; Int_v(p*4)) 
    in
    (if (!debug_flag) then 
      print_string ("eval_exp "^(exp2string e)^" yields "^(val2string res)^"\n")
    else ());
    dump_mem env;
    res

(* Evaluate a Cish statement *)
and eval_stmt ((s:rstmt),(pos:int)) (env:env) : unit = 
    match s with 
      Exp e -> let _ = eval_exp e env in ()
    | Seq(s1,s2) -> (eval_stmt s1 env; eval_stmt s2 env)
    | If(e,s1,s2) -> 
      if (eval_int e env) <> 0 then 
        eval_stmt s1 env 
      else 
        eval_stmt s2 env
    | While(e,s1) -> 
      eval_stmt (If(e,(Seq(s1,(s,pos)),pos),(skip,pos)),pos) env
    | For(e1,e2,e3,s1) ->
        let _ = eval_exp e1 env in
        eval_stmt (While(e2, (Seq(s1,(Exp e3,pos)),pos)), pos) env
    | Return e -> raise (Done (eval_exp e env))
    | Let(x,e,s) -> 
      let v = eval_exp e env in
      let env = insert x v env in
      eval_stmt s env

(* Evaluate a Cish program by invoking main() *)
let eval (p:program):value = 
   let env = List.fold_left insert_fn (empty_env()) p in
   let Fn (f) = lookup_fn "main" env in
   try (eval_stmt f.body env; Int_v 0) with Done v -> v
