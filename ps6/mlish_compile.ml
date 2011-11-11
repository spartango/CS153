module ML = Mlish_ast
module S = Scish_ast

exception Impossible
let rec compile_exp ((e,_):ML.exp) : S.exp = 
    match e with
      ML.Var x -> S.Var x
    | ML.Fn(x,e) -> S.Lambda(x,compile_exp e)
    | ML.App(e1,e2) -> S.App(compile_exp e1, compile_exp e2)
    | ML.If(e1,e2,e3) -> S.If(compile_exp e1, compile_exp e2, compile_exp e3)
    | ML.Let(x,e1,e2) -> S.sLet x (compile_exp e1) (compile_exp e2)
    | ML.PrimApp(ML.Int i,_) -> S.Int i
    | ML.PrimApp(ML.Bool false,_) -> S.Int 0
    | ML.PrimApp(ML.Bool true,_) -> S.Int 1
    | ML.PrimApp(ML.Unit,_) -> S.Int 0
    | ML.PrimApp(ML.Nil,_) -> S.Int 0
    | ML.PrimApp(ML.Plus,es) -> S.PrimApp(S.Plus,List.map compile_exp es)
    | ML.PrimApp(ML.Minus,es) -> S.PrimApp(S.Minus,List.map compile_exp es)
    | ML.PrimApp(ML.Times,es) -> S.PrimApp(S.Times,List.map compile_exp es)
    | ML.PrimApp(ML.Div,es) -> S.PrimApp(S.Div,List.map compile_exp es)
    | ML.PrimApp(ML.Eq,es) -> S.PrimApp(S.Eq,List.map compile_exp es)
    | ML.PrimApp(ML.Lt,es) -> S.PrimApp(S.Lt,List.map compile_exp es)
    | ML.PrimApp(ML.Pair,es) -> S.PrimApp(S.Cons,List.map compile_exp es)        
    | ML.PrimApp(ML.Fst,es) -> S.PrimApp(S.Fst,List.map compile_exp es)
    | ML.PrimApp(ML.Snd,es) -> S.PrimApp(S.Snd,List.map compile_exp es)
    | ML.PrimApp(ML.Cons,es) -> S.PrimApp(S.Cons,List.map compile_exp es)
    | ML.PrimApp(ML.Hd,es) -> S.PrimApp(S.Fst,List.map compile_exp es)
    | ML.PrimApp(ML.Tl,es) -> S.PrimApp(S.Snd,List.map compile_exp es)
    | ML.PrimApp(ML.IsNil,[e]) -> S.If(compile_exp e, S.Int 0, S.Int 1)
    | ML.PrimApp(ML.IsNil,_) -> raise Impossible      
