(* An interpreter for Scish *)
open Scish_ast

(* A closure is a function and its environment *)
type closure = { body : (var * exp); env : env }
(* Values are either integers, closures, or pairs *)
and value = 
  Int_v of int
| Cons_v of value * value
| Closure_v of closure        
(* an environment provides values for the variables in scope.
 * It's essentially a list of frames, corresponding to each
 * nested lambda. *)
and env = 
  Empty
| Frame of (var * value) * env

(* Pretty-printing for values *)
let rec val2string (v:value):string =
    match v with
      Int_v i -> string_of_int i
    | Closure_v { env=_; body=_ } -> "#closure"
    | Cons_v (v1,v2) -> "("^(val2string v1)^" "^(val2string v2)^")"

exception Error
let error s = (print_string s; print_string "\n"; raise Error)

(* Lookup a variable in the environment -- return the 1st binding
 * corresponding to the variable. *)          
let rec lookup (x:var) (env:env) : value = 
    match env with
      Empty -> error ("Unbound variable "^x)
    | Frame ((y,v),rest) -> if (y = x) then v else lookup x rest

(* Evaluate e in environment env to a value *)          
let rec eval (e:exp) (env:env) : value = 
    match e with
      Int i -> Int_v i
    | If(e1,e2,e3) -> 
      (match (eval e1 env) with
         Int_v(0) -> eval e3 env
       | _ -> eval e2 env)
    | Var x -> lookup x env
    | PrimApp(p,es) -> prim_apply(p,List.map (fun e -> eval e env) es)
    | Lambda(x,e) -> 
        (* We don't evaluate lambda body (since we don't yet have the
         * arguments) but must record the environment so that we have
         * bindings for the free variables. *)
        Closure_v{body=(x,e);env=env}
    | App(e1,e2) ->
      let (v1,v2) = (eval e1 env, eval e2 env) in
        match v1 with
           Closure_v{body=(x,e);env=closure_env} ->
           (* We extend the closure's environment with a new frame, mapping
            * the lambda-bound variable to the argument v2. *)
           let new_env = Frame((x,v2),closure_env) in
             (* We evaluate the body of the closure under the extended
              * environment.  Note that our original environment, env,
              * is *not* used. *)
           eval e new_env
         | v -> error ("Attempted to apply "^(val2string v))
        
(* Implements the primitive operations *)
and prim_apply((p:primop), (vs:value list)) : value = 
    match (p,vs) with
      (Plus,[Int_v(i);Int_v(j)]) -> Int_v(i+j)
    | (Minus,[Int_v(i);Int_v(j)]) -> Int_v(i-j)
    | (Times,[Int_v(i);Int_v(j)]) -> Int_v(i*j)
    | (Div,[Int_v(i);Int_v(j)]) -> Int_v(i / j)
    | (Cons,[hd;tl]) -> Cons_v(hd,tl)
    | (Fst,[Cons_v(f,s)]) -> f
    | (Snd,[Cons_v(f,s)]) -> s
    | (Eq,[Int_v(i);Int_v(j)]) -> 
      if (i = j) then Int_v(1) else Int_v(0)
    | (Lt,[Int_v(i);Int_v(j)]) -> 
      if (i < j) then Int_v(1) else Int_v(0)
    | (_,vs) -> 
      error ("Attempted to apply "^(primop2string p)^" to "^
             (String.concat " " (List.map val2string vs)))

let run (e:exp) = eval e Empty

