(* Defines our Monadic Intermediate Form, conversion of Scish AST to
 * Monadic Form, and various optimizations on Monadic Form. *)
module S = Scish_ast
open Monad
open Utility

exception TODO
exception IntAsFunction
exception EXTRA_CREDIT

(* Zips lets together *)
let rec flatten (x: var) (e1: exp) (e2: exp) : exp =
    match e1 with
        (* Set value of x to the return value of the expression *)
        | Return o -> LetVal(x, Op o, e2)
        | LetVal(y, v, e) -> LetVal(y, v, flatten x e e2) 
        | LetCall(y, f, ws, e) ->
              LetCall(y, f, ws, flatten y e e2)
        | LetIf(y, w, e1, e2, e_next) ->
              LetIf(y, w, e1, e2, flatten x e_next e2)



(* used to generate fresh variables *)
let counter = ref 0
let fresh_var() = 
    let c = !counter in
    counter := c+1; "x"^(string_of_int c)

(* naive var->var environments *)
exception UnboundVariable of string
let empty_env (x:var):var = (print_string ("unbound variable "^x); 
                             raise (UnboundVariable x))
let extend (env:var->var) (x:var) (y:var) =
    fun z -> if z = x then y else env z

(* convert an expression e to monadic form:
 *   env is used to rename variables on the fly so they are unique.
 *   k is used as a continuation as explained below:
 * 
 * Conceptually, each Scish expression compiles down to a
 * monadic expression of the form Let(x1,v1,...,Let(xn,vn),Return v).
 * That is, it's a sequence of let-bindings followed by a return
 * of an operand.
 *
 * Consider what happens when we have a compound expression
 * such as e1+e2.  If we had nested lets, then we could write:
 *    Let(x1,compile e1,
 *    Let(x2,compile e2,
 *    LetVal(x3,PrimApp(Plus,[Var x1,Var x2]),
 *    Return (Var x3))))
 * but we don't have nested lets.  In general, we'll have to
 * take the let-expressions and operand for e1 and flatten it
 * out using the equation:
 *
 *  (let x = (let y = a in b) in c) == let y = a in let x = b in c
 *
 * Similarly, we'll have to flatten out the translation of e2.
 * But all of this flattening amounts to appending lists of 
 * let-declarations.  If we're not careful, we end up with a 
 * quadractic algorithm (the same problem we had with lowering to MIPS.)
 *
 * To solve this problem, we use a clever trick which allows us
 * to flatten on-the-fly.  The trick is to parameterize the 
 * translation with a function, which when given an operand,
 * generates "the rest of the translation".  This extra parameter,
 * called a continuation, is essentially a functional accumulator.
 *
 * Consider the case for tom App(e1,e2) Return
 * Imagine that the monadic form of the expressions is
 *    e1 == Let x1=a1 in...Let xn=an in Return w1
 *    e2 == Let y1=b1 in...Let ym=bm in Return w2
 * Then we should get as output something like:
 *   Let x1=a1...in Let xn=an in 
 *    Let y1=b1 ...in Let ym=bm in Let z=w1(w2) in Return z
 *
 * Following the definitions, we have:
 *   tom (App(e1,e2)) Return = tom e1 k1
 *     where k1 = (fn w1-> tom e2 (fn w2 -> Let z=w1(w2) in Return z))
 *   tom e1 k1 = 
 *     Let x1=a1 in ... Let xn=an in k1(w1)    (by assumption)
 *
 *   k1(w1) = tom e2 (fn w2 -> Let z=w1(w2) in Return z)
 *          = Let y1=b1 in ... Let ym=bm in Let z=w1(w2) in Return z
 * So tom e1 k1 = 
 *     Let x1=a1 in ... Let xn=an in 
 *       Let y1=b1 in ...Let ym=bm in Let z=w1(w2) in Return z
 *)
let rec tom (e : S.exp) (env:var->var) (k : operand -> exp) : exp = 
    match e with
      S.Var x -> k (Var (env x))
    | S.Int i -> k (Int i)
    | S.App(S.Lambda(x,e1),e2) -> (* same as let x = e2 in e1 *)
        let x' = fresh_var() in
        tom e2 env (fun w1 -> LetVal(x',Op w1,tom e1 (extend env x x') k))
    | S.App(e1,e2) -> 
        let x = fresh_var() in
        tom e1 env (fun w1 -> tom e2 env (fun w2 -> LetCall(x,w1,w2,k(Var x))))
    | S.Lambda(x,e) -> 
        let x' = fresh_var() in
        let f = fresh_var() in
        LetVal(f,Lambda(x',tom e (extend env x x') (fun x -> Return x)),k(Var f))
    | S.PrimApp(p,es) -> 
        let x = fresh_var() in
        toms es [] env (fun ws -> LetVal(x,PrimApp(p,ws),k(Var x)))
    | S.If(e1,e2,e3) -> 
        let x = fresh_var() in
        tom e1 env 
              (fun w -> LetIf(x,w,tom e2 env (fun x -> Return x),
                              tom e3 env (fun x -> Return x),k(Var x)))
and toms (es : S.exp list) (accum: operand list) 
           (env : var->var) (k: operand list -> exp) = 
    match es with
      [] -> k(List.rev accum)
    | e::rest -> tom e env (fun w -> toms rest (w::accum) env k)

let tomonadic (e:S.exp) : exp = tom e empty_env (fun x -> Return x)

(* a flag used to track when an optimization makes a reduction *)
let changed : bool ref = ref true
let change x = (changed := true; x)


let compare_operands op1 op2 =
  match (op1, op2) with
  | (Var(v), Int(i))  -> 1
  | (Int(i), Var(v))  -> -1
  | (Var(v), Var(v2)) -> String.compare v v2
  | (Int(i), Int(i2)) -> compare i i2

(* Sorts commutative ops in an expression to allow for 
 * More hits in cse env look ups *)
let sort_ops (v : value) : value =
  match v with
  | PrimApp(op, operands) -> PrimApp(op, (List.sort compare_operands operands))
  | _ -> v 

(* naive 'a -> 'b option environments *)
let empty_env x = None
let extend env x w = fun y -> if y = x then Some w else env y

(* operand propagation -- LetVal(x,Op w,e) --> e[w/x] -- just like notes. *)
let rec cprop_exp (env : var -> operand option) (e:exp) = 
    match e with
      Return w -> Return (cprop_oper env w)
    | LetVal(x,Op w,e) -> 
        change(cprop_exp (extend env x (cprop_oper env w)) e)
    | LetVal(x,PrimApp(p,ws),e) -> 
        LetVal(x,PrimApp(p,List.map (cprop_oper env) ws),cprop_exp env e)
    | LetVal(x,Lambda(y,e1),e2) -> 
        LetVal(x,Lambda(y,cprop_exp env e1),cprop_exp env e2)
    | LetCall(x,w1,w2,e) -> 
        LetCall(x,cprop_oper env w1, cprop_oper env w2,cprop_exp env e)
    | LetIf(x,w,e1,e2,e) -> 
        LetIf(x,cprop_oper env w,cprop_exp env e1,cprop_exp env e2,
              cprop_exp env e)
and cprop_oper (env : var -> operand option) (w:operand) = 
    match w with
      Var x -> (match env x with None -> w | Some w' -> w')
    | Int _ -> w

let cprop e = cprop_exp empty_env e

(* common sub-value elimination -- as in the slides *)

let cse (e : exp) : exp =
  let rec cse_exp(env:value->var option)(e:exp):exp =
    match e with
      | Return w -> Return w
      | LetVal(x,v,e) ->
        let v = sort_ops v in
        (match env v with
        | None -> LetVal(x,cse_val env v,
                            cse_exp (extend env v x) e)
        | Some y -> LetVal(x,Op(Var y),cse_exp env e))
      | LetCall(x,f,w,e) -> LetCall(x,f,w,cse_exp env e)
      | LetIf(x,w,e1,e2,e) ->
          LetIf(x,w,cse_exp env e1,cse_exp env e2,
                cse_exp env e)
  and cse_val env v =
    match v with | Lambda(x,e) ->  Lambda(x,cse_exp env e)
                 | v -> v
  in
  cse_exp empty_env e

(* constant folding
 * Apply primitive operations which can be evaluated. e.g. fst (1,2) = 1
 *)
(*
type value = 
  Op of operand
| PrimApp of S.primop * (operand list)
| Lambda of var * exp
      *)

(* Constant folding optimizations on plus *)
let plus_ops (args: operand list) : value = 
    match args with
        (* For + 0, just return operand *)
        | [Int(0); v] -> Op v
        | [v; Int(0)] -> Op v
        | [Int(i); Int(u)] -> Op(Int(i + u))
        | _ -> PrimApp(S.Plus, args)

let minus_ops (args: operand list) : value =
    match args with
        | [v; Int 0] -> Op v
        | [Int(i); Int(u)] -> Op(Int(i - u))
        | [v1; v2] when (v1 = v2) ->  Op(Int(0))
        | _ -> PrimApp(S.Minus, args)

let times_ops (args: operand list) : value =
    match args with
        | [v; Int 0] -> Op(Int 0)
        | [v; Int 1] -> Op v
        | [Int 0; v] -> Op(Int 0)
        | [Int 1; v] -> Op v
        | [Int(i); Int(u)] -> Op(Int (i * u))
        | _ -> PrimApp(S.Times, args)

let div_ops (args: operand list) : value =
    match args with
        | [v; Int 1] -> Op v
        | [Int 0; v] -> Op (Int 0)
        | [Int i; Int u] -> Op(Int (i / u))
        | [v1; v2] when (v1 = v2) ->Op(Int(1))
        | _ -> PrimApp(S.Div, args)

let eq_ops (args: operand list) : value =
    match args with
        (* Return 1 or 0 if args are both ints *)
        | [Int i; Int u] ->
              if i = u
              then Op (Int 1)
              else Op (Int 0)
        (* If two values are the same, condense *)
        | [v1; v2] ->
              (* CHECK that this works *)
              if v1 = v2
              then Op (Int 1)
              else Op (Int 0)
        | _ -> PrimApp(S.Eq, args)

let lt_ops (args: operand list) : value =
    match args with
        | [Int i; Int u] ->
              if i < u
              then Op (Int 1)
              else Op (Int 0)
        (* If two values are the same, one is not less than the other *)
        | [v1; v2] ->
              if v1 = v2
              then Op (Int 0)
              else PrimApp(S.Lt, args)
        | _ -> PrimApp(S.Lt, args)

let fst_ops (args: operand list) : value =
    match args with
        (* Simplify to first part of pair *)
        | [v1; v2] -> Op v1
        | _ -> PrimApp(S.Fst, args)

let snd_ops (args: operand list) : value =
    match args with
        (* Simplify to first part of pair *)
        | [v1; v2] -> Op v2
        | _ -> PrimApp(S.Fst, args)

let rec cfold (e : exp) : exp = 
    match e with
        | Return o -> Return o
        | LetVal (x, v, e) -> LetVal(x, cfold_val v, cfold e)
        | LetCall (x, f, ws, e) -> 
              (* Check that this is correct *)
              LetCall (x, f, ws, cfold e)
        (* Remove the if when it is either true or false *)
        | LetIf (x, (Int 1), e1, e2, e_next) ->
              cfold (flatten x e1 e_next)
        | LetIf (x, (Int 0), e1, e2, e_next) ->
              cfold (flatten x e2 e_next)
        | LetIf (x, w, e1, e2, e_next) ->
              LetIf(x, w, cfold e1, cfold e2, cfold e_next)

and cfold_val (v: value) : value = 
    (match v with
        (* Return an operand as is *)
        | Op o -> Op o
        (* Optimize primative operations *)
        | PrimApp (p_op, args) ->
              (match p_op with
                  | S.Plus -> plus_ops args 
                  | S.Minus -> minus_ops args
                  | S.Times -> times_ops args
                  | S.Div -> div_ops args
                  | S.Eq -> eq_ops args
                  | S.Lt -> lt_ops args
                  (* Nothing to do for cons - will be eliminated by dce *)
                  | S.Cons -> PrimApp(S.Cons, args)
                  | S.Fst -> fst_ops args
                  | S.Snd -> snd_ops args)
        (* Fold constants in functions, returning function *)
        | Lambda (v, e) -> Lambda(v, cfold e))

(* To support a somewhat more efficient form of dead-code elimination and
 * inlining, we first construct a table saying how many times each variable 
 * is used, and how many times each function is called.
 * This table is then used to reduce LetVal(x,v,e) to e when x is used
 * zero times, and to reduce LetVal(x,Lambda(y,e),...,LetCall(z,x,w,e2)...)
 * to (...LetVal(y,Op w,(Let(z,e,e2)))...) when x is used once and 
 * that use is a call site.
 *)
(* type cnt_table = (var,{uses:int ref,calls:int ref}) Hashtbl.hash_table *)
type entry = { uses : int ref; calls: int ref }
exception NotFound
let new_cnt_table() = 
    Hashtbl.create 101
let def (t) (x:var) = 
    Hashtbl.add t x {uses=ref 0;calls=ref 0}
let inc r = (r := !r + 1)
let use (t) (x:var) = 
    inc ((Hashtbl.find t x).uses)
let call (t) (x:var) = 
    inc ((Hashtbl.find t x).calls)
let get_uses (t) (x:var) : int = !((Hashtbl.find t x).uses)
let get_calls (t) (x:var) : int = !((Hashtbl.find t x).calls)

(* Returns a hash table *)
let count_table (e:exp) = 
    let table = new_cnt_table() in
    let def = def table in
    let use = use table in
    let call = call table in
    let rec occ_e e = 
      match e with
        Return w -> occ_o w
      | LetVal(x,v,e) -> (def x; occ_v v; occ_e e)
      | LetCall(x,Var f,w2,e) -> 
         (def x; use f; call f; occ_o w2; occ_e e)
      | LetCall(x,w1,w2,e) -> 
         (def x; occ_o w1; occ_o w2; occ_e e)
      | LetIf(x,w,e1,e2,e) -> (def x; occ_o w; occ_e e1; occ_e e2;
                               occ_e e)
    and occ_v v = 
      match v with
          |  Op w -> occ_o w
          | PrimApp(_,ws) ->  List.iter occ_o ws
          | Lambda(x,e) -> (def x; occ_e e)
        
    and occ_o oper = 
      match oper with
        Var x -> use x
      | Int _ -> () in
    occ_e e; table


(* dead code elimination *)
let dce (e:exp) : exp = 
    let ct_table = count_table e in
    let rec dce_r lam_map (e: exp) : exp =
        let dce_passed_map = dce_r lam_map in
        match e with
            | Return o -> Return o
            | LetVal(x, v, next_e) ->
                  (* If the value is unused, eliminate it and dce the next expression *)
                  if (get_uses ct_table x) = 0
                  then dce_r lam_map next_e
                  else 
                      (* Check if value is function, if it called once *)
                      (match v with
                          | Lambda(y, body) ->
                                (* Eliminate dead code in body of lambda - does not need to get back
                                   lam_map because code in body is not in scope of rest of program *)
                                (* let body1 = dce_r lam_map body in *)
                                if (get_calls ct_table x) = 1
                                (* Remove function map of vars to functions - body optimized before inserted into map*)
                                then
                                    let _ = print_endline (exp2string body) in
                                    (dce_r (StringMap.add x (y, body) lam_map) next_e)
                                else LetVal(x, Lambda(y, body), dce_passed_map next_e)
                          (* Remove nothing if value is operand or primapp *)
                          | _ -> LetVal(x, v, dce_passed_map next_e))
            | LetCall(x, f, ws, next_e) ->
                  (* Look to see if function has been previously removed and insert body before call if so *)
                  let f_name = match f with
                      | Var name -> 
                            let _ = print_endline (name) in
                            name
                      | Int _ -> raise IntAsFunction in
                      if (StringMap.mem f_name lam_map)
                      then
                      (* Get lambda out of map *)
                      let (param, body) = StringMap.find f_name lam_map in
                          LetVal(param, Op(ws), flatten x body (dce_passed_map next_e))
                  else LetCall(x, f, ws, dce_passed_map next_e)
            | LetIf(x, o1, e1, e2, next_e) ->
                  LetIf(x, o1, dce_passed_map e1, dce_r lam_map e2, dce_passed_map next_e) in
        dce_r StringMap.empty e
                              
                          
                           

(* (1) inline functions 
 * (2) reduce LetIf expressions when the value being tested is a constant.
 * 
 * In both cases, we are forced to re-flatten out what would otherwise
 * be nested let-expressions.  Therefore, we use the "splice" helper
 * function to splice the two expressions together.   In particular,
 * splice x e1 e2 is equivalent to flattening out let x=e1 in e2.
 * Note, however, that in the case of inlining where the threshold is
 * above 1, we can end up duplicating the body of a function.  We must
 * restore the invariant that no bound variable is duplicated by renaming
 * each bound variable in the copy (else other optimizations will break
 * due to variable capture.)  
 *)
(* It's necessary to use splice because the same function could potentially be
   inlined multiple times - which would lead to variable capture. As long as a
   function is only inlined once - i.e. dce - it is ok to use flatten *)
let splice x e1 e2 = 
    let rec splice_exp final (env : var -> operand option) (e:exp) = 
      match e with
        Return w -> 
          if final then LetVal(x,Op (cprop_oper env w),e2)
          else Return(cprop_oper env w)
      | LetVal(y,v,e) -> 
          let y' = fresh_var() in
          LetVal(y',loop_value env v,
                splice_exp final (extend env y (Var y')) e)
      | LetCall(y,w1,w2,e) -> 
          let y' = fresh_var() in
          LetCall(y',cprop_oper env w1,cprop_oper env w2,
                  splice_exp final (extend env y (Var y')) e)
      | LetIf(y,w,e1,e2,e) -> 
          let y' = fresh_var() in
          LetIf(y',cprop_oper env w, splice_exp false env e1,
                splice_exp false env e2, 
                splice_exp final (extend env y (Var y')) e)
    and loop_value env v = 
      match v with
        Op w -> Op (cprop_oper env w)
      | Lambda(y,e) -> 
          let y' = fresh_var() in 
          Lambda(y',splice_exp false (extend env y (Var y')) e)
      | PrimApp(p,ws) -> PrimApp(p,List.map (cprop_oper env) ws) in
  splice_exp true empty_env e1

let always_inline_thresh (e : exp) : bool = true  (** Always inline **)
let never_inline_thresh  (e : exp) : bool = false (** Never inline  **)

(* return true if the expression e is smaller than i, i.e. it has fewer
 * constructors
 *)
let size_inline_thresh (i : int) (e : exp) : bool = 
    (* Sizes operand - allows for us to easily change how this is counted *)
    let size_var = 0 in
    let rec size_operand (o: operand) : int =
            1
    and size_value (v: value) : int =
        match v with
            (* 1 for Op constructor, size of operand *)
            | Op o -> 1 + size_operand o
            (* 1 for PrimApp constructor, 1 for actual primative op, size of operands *)
            | PrimApp (p, os) -> 1 + 1 + (List.fold_left (fun a o -> a + (size_operand o)) 0 os)
            (* 1 for Lambda, size of var, size of expression *)
            | Lambda (y, e) -> size (1 + size_var) e
    and size (count: int) (e: exp)  : int =
        match e with
            | Return o -> count + 1 + (size_operand o)
             (* 1 for LetVal, size of value, size of expression *)
            | LetVal(x, v, e) -> size (count + 1 + (size_var) + (size_value v)) e
            | LetCall(x, f, ws, e) -> size (count + 1 + (size_var) + (size_operand f) + (size_operand ws)) e
            | LetIf(x, t, e1, e2, e_next) ->
                  size (size (size (count + 1 + (size_var) + (size_operand t)) e1) e2) e_next in
    ((size 0 e) < i)
(* inlining 
 * only inline the expression e if (inline_threshold e) return true.
 *)
let inline (inline_threshold: exp -> bool) (e:exp) : exp = 
    (* INVARIANT: body of function only added to lambda map if it is below the inline threshold *)
    let rec inline_r lam_map (ex: exp) : exp = 
        let inline_passed_map = inline_r lam_map in
        match ex with
            (* Return unchanged *)
            | Return o -> ex
            | LetVal(x, v, e_next) ->
                  (* Check if is function declaration *)
                  (match v with
                      (* Potentially prep for inline if Lambda *)
                      | Lambda(y, body) ->
                            (* Inline in body of lambda *)
                            let body1 = inline_r lam_map body in
                            let lam_map1 = 
                                (* Add body to map if below inline threshold - does not remove original let binding *)
                                if (inline_threshold body)
                                (* CHECK - I believe this is necessary to make splice work correctly. It allows the parameter to be renamed each time it is inlined *)
                                (* Key is LetVal variable; value is "holey expression" where the hole is the value of the parameter *)
                                then StringMap.add x (fun ws -> (LetVal(y, ws, body1))) lam_map
                                else lam_map in
                                LetVal(x, Lambda(y, body1), inline_r lam_map1 e_next)
                      (* Otherwise, continue through code *)
                      | _ -> LetVal(x, v, inline_passed_map e_next))
            | LetCall(x, f, ws, e_next) ->
                  (* Helper to convert operand to string *)
                  let f_name = match f with
                      | Var name -> name
                      | Int i -> string_of_int i in
                      (* Inline if function in lambda map *)
                      if (StringMap.mem f_name lam_map)
                      then
                          (* Get body out of map - in form of holey-exp where hole is value of param *)
                          let body = StringMap.find f_name lam_map in
                          (* Splice in inlined code *)
                              splice x (body (Op ws)) (inline_passed_map e_next)
                      else 
                          (* Otherwise, do not inline this function and continue inlining *)
                          LetCall(x, f, ws, inline_passed_map e_next)
            (* CHECK: Assumes that true is always 1 and false is always 0 *)
            | LetIf(x, t, e1, e2, e_next) ->
                  (* Potentially eliminate branches if t is always true or false while inlining branchs.
                   * Since functions in one branch not in scope in other, no need to get lam_maps back from branches *)
                  match t with
                      (* Always true *)
                      | Int(1) ->
                            (* Remove false branch - x gets result of e1 *)
                            splice x (inline_passed_map e1) (inline_passed_map e_next)
                      (* Always false *)
                      | Int(0) ->
                            (* Removre true brnach - x gets result of e2 *)
                            splice x (inline_passed_map e2) (inline_passed_map e_next)
                      (* Undetermined *)
                      | _ ->
                            LetIf(x, t, inline_passed_map e1, inline_passed_map e2, inline_passed_map e_next) in
        inline_r StringMap.empty e
        

(* reduction of conditions
 * - Optimize conditionals based on contextual information, e.g.
 *   if (x < 1) then if (x < 2) then X else Y else Z =-> 
 *     if (x < 1) then X else Z
 *   (since x < 1 implies x < 2)
 * - This is similar to constant folding + logic programming
 *)
let redtest (e:exp) : exp = e
 

(* optimize the code by repeatedly performing optimization passes until
 * there is no change. *)
let optimize inline_threshold e = 
    let opt = fun x -> dce (cprop (redtest (cse (cfold ((inline inline_threshold) x))))) in 
    let rec loop (i:int) (e:exp) : exp = 
      (if (!changed) then 
        let _ = changed := false in
        let e' = opt e in
        let _ = print_string ("\nAfter "^(string_of_int i)^" rounds:\n") in
        let _ = print_string (exp2string e') in
        loop (i+1) e'
      else e) in
    changed := true;
    loop 1 e

