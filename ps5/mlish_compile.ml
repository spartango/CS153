module ML = Mlish_ast
module S = Scish_ast

exception ImplementMe
let rec compile_exp ((e,_):ML.exp) : S.exp = 
match e with
    (* Return Scish variable *)
    | ML.Var(v) -> S.Var(v)
    (* Compile primative operation *)
    | ML.PrimApp(prim, es) -> raise ImplementMe
    (* Build to Scish lambda *)
    | ML.Fn(param, body) -> S.Lambda(param, (compile_exp body))
    (* Apply function to an argument *)
    | ML.App (func, arg) -> S.App((compile_exp func), compile_exp arg)
    (* If control statement - compile to Scish If *)
    | ML.If(e1, e2, e3) -> S.If(compile_exp e1, compile_exp e2, compile_exp e3)
    (* Use Scish sLet, or apply lambda of e2 to e1 *)
    | ML.Let(v, e1, e2) -> S.App(S.Lambda(v, compile_exp e2), compile_exp e1) 


