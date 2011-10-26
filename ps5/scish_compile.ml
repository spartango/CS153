(* TODO:  your job is to map ScishAst expressions to CishAst functions. 
   The file sample_input.scish shows a sample Scish expression and the
   file sample_output.cish shows the output I get from my compiler.
   You will want to do your own test cases...
 *)

exception Unimplemented
exception FatalError 

(* generate fresh labels *)
let label_counter = ref 0
let new_int counter = (counter := (!counter) + 1; !counter)
let new_label() = "t"^(string_of_int (new_int label_counter))

(* holds all the functions we will encounter *)
let func_list = ref []

(* use these to build Cish_ast expressions *)
let c_int i = (Cish_ast.Int i,0)
let c_var x = (Cish_ast.Var x,0)
let c_binop e1 b e2 = (Cish_ast.Binop(e1,b,e2),0)
let c_not e = (Cish_ast.Not e,0)
let c_and e1 e2 = (Cish_ast.And(e1,e2),0)
let c_or e1 e2 = (Cish_ast.Or(e1,e2),0)
let c_assign x e = (Cish_ast.Assign(x,e),0)
let c_call e elst = (Cish_ast.Call(e,elst),0)
let c_load e = (Cish_ast.Load e,0)
let c_store e1 e2 = (Cish_ast.Store(e1,e2),0)
let c_malloc e = (Cish_ast.Malloc e,0)

let c_exp e = (Cish_ast.Exp e,0)
let c_seq s1 s2 = (Cish_ast.Seq(s1,s2),0)
let c_if e s1 s2 = (Cish_ast.If(e,s1,s2),0)
let c_return e = (Cish_ast.Return e,0)
let c_let x e s = (Cish_ast.Let(x,e,s),0)

(* convienient bindings *)
let result = c_var "result" (* holds results of computation *)
let dynenv = c_var "dynenv" (* environment to generated functions *)
let zero = c_int 0
let four = c_int 4
let eight = c_int 8

(* environment functions *)
let lookup x lst =
    let rec lookup_h x lst acc =
      match lst with
        [] -> raise FatalError
      | (y::tl) -> if (y=x) then c_load acc
          else lookup_h x tl (c_load (c_binop acc Cish_ast.Plus four)) in
    match lst with
      [] -> raise FatalError
    | y::tl -> if (y=x) then c_load dynenv 
        else lookup_h x tl (c_load (c_binop dynenv Cish_ast.Plus four))

let insert x lst = x::lst

(* a closure is a 2 word structure *)
let make_closure value env =
    let malloc = c_exp (c_assign "result" (c_malloc eight)) in
    let load1 = c_exp (c_store result (c_var value)) in
    let load2 = c_exp (c_store (c_binop result Cish_ast.Plus four) (c_var env)) in
    c_seq malloc (c_seq load1 load2)

(* invariant: result of computation is in "result" *)
let rec compile_stmt (e:Scish_ast.exp) env : Cish_ast.stmt  =
    match e with
      Scish_ast.Int i -> c_exp (c_assign "result" (c_int i))
    | Scish_ast.Var x -> c_exp (c_assign "result" (lookup x env))
    | Scish_ast.PrimApp (p,elst) -> compile_prim p elst env
    | Scish_ast.Lambda (x,e1) ->
        let e1' = compile_stmt e1 (insert x env) in
        let body = c_let "result" zero (c_seq e1' (c_return result)) in
        let fun_label = new_label() in
        let func_c = { Cish_ast.name = fun_label;
                       Cish_ast.args = ["dynenv"];
                       Cish_ast.body = body;
                       Cish_ast.pos = 0 } in
        let _ = func_list := (Cish_ast.Fn(func_c))::(!func_list) in
        make_closure fun_label "dynenv"
    | Scish_ast.App (e1,e2) ->
        let fun_label = new_label() in
        let env_label = new_label() in
        let arg_label = new_label() in
        let e1' = compile_stmt e1 env in 
        let e2' = compile_stmt e2 env in
        let t1 = c_exp (c_assign fun_label (c_load result)) in
        let t2 = c_exp (c_assign env_label (c_load (c_binop result Cish_ast.Plus four))) in
        let t3 = c_seq e2' (c_exp (c_assign arg_label result)) in
        let t4 = make_closure arg_label env_label in
        let t5 = c_exp (c_assign "result" (c_call (c_var fun_label) [result])) in
        let t6 = c_seq t1 (c_seq t2 (c_seq t3 (c_seq t4 t5))) in
        let arg_stmt = c_let arg_label zero (c_seq e1' t6) in
        let env_stmt = c_let env_label zero arg_stmt in
        let fun_stmt = c_let fun_label zero env_stmt in
        fun_stmt
    | Scish_ast.If (e1,e2,e3) ->
        let (s1,_) = compile_stmt e1 env in
        let s2 = compile_stmt e2 env in
        let s3 = compile_stmt e3 env in
        match s1 with
          Cish_ast.Exp s1' -> c_if s1' s2 s3
        | _ -> raise FatalError

(* invariant: result of computation is in "result" *)
and compile_prim p elst env =
    let compile_nested e1 e2 op =
      let e1' = compile_stmt e1 env in
      let e2' = compile_stmt e2 env in
      let tmp_label = new_label() in
      let t1 = c_exp (c_assign tmp_label result) in
      let t2 = c_exp (c_assign "result" (c_binop (c_var tmp_label) op result)) in
      c_let tmp_label zero (c_seq e1' (c_seq t1 (c_seq e2' t2))) in
    match p,elst with
      Scish_ast.Plus,[e1;e2] -> compile_nested e1 e2 Cish_ast.Plus 
    | Scish_ast.Minus,[e1;e2] -> compile_nested e1 e2 Cish_ast.Minus
    | Scish_ast.Times,[e1;e2] -> compile_nested e1 e2 Cish_ast.Times
    | Scish_ast.Div,[e1;e2] -> compile_nested e1 e2 Cish_ast.Div
    | Scish_ast.Cons,[e1;e2] -> 
        let e1' = compile_stmt e1 env in
        let e2' = compile_stmt e2 env in
        let tmp1_label = new_label() in
        let tmp2_label = new_label() in
        let t1 = c_exp (c_assign tmp1_label result) in
        let t2 = c_exp (c_assign tmp2_label result) in
        let t3 = make_closure tmp1_label tmp2_label in
        let t4 = c_let tmp2_label zero (c_seq e1' (c_seq t1 (c_seq e2' (c_seq t2 t3)))) in
        c_let tmp1_label zero t4 
    | Scish_ast.Fst,[e1] -> 
        let e1' = compile_stmt e1 env in
        c_seq e1' (c_exp (c_assign "result" (c_load result)))
    | Scish_ast.Snd,[e2] -> 
        let e2' = compile_stmt e2 env in
        c_seq e2' (c_exp (c_assign "result" (c_load (c_binop result Cish_ast.Plus four))))
    | Scish_ast.Eq,[e1;e2] -> compile_nested e1 e2 Cish_ast.Eq
    | Scish_ast.Lt,[e1;e2] -> compile_nested e1 e2 Cish_ast.Lt
    | _,_ -> raise FatalError

let rec compile_exp (e:Scish_ast.exp) : Cish_ast.program =
    let e' = compile_stmt e [] in
    let dynenv = c_seq (c_let "dynenv" zero e') (c_return result) in
    let body = c_let "result" zero dynenv in
    let func_c = { Cish_ast.name = "main";
                   Cish_ast.args = [];
                   Cish_ast.body = body;
                   Cish_ast.pos = 0 } in
    ((Cish_ast.Fn(func_c))::(!func_list))
