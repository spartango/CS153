module ML = Mlish_ast
module S = Scish_ast

exception ImplementMe
let rec compile_exp ((e,_):ML.exp) : S.exp = 
match e with
    | ML.Var(v)
    | ML.PrimApp(prim, es)
    | ML.Fn(param, body)
    | ML.App (func, arg)
    | ML.If(e1, e2, e3)
    | ML.Let(v, e1, e2)

raise ImplementMe
