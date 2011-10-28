module ML = Mlish_ast
module S = Scish_ast

exception ImplementMe
exception InvalidNumberParameters
let rec compile_exp ((e,_):ML.exp) : S.exp = 
match e with
    (* Return Scish variable *)
    | ML.Var(v) -> S.Var(v)
    (* Compile primative operation *)
    | ML.PrimApp(prim, exps) ->
          let verify_zero_arg () : unit =
              if (List.length exps) = 0 then () else raise InvalidNumberParameters in
          let verify_single_arg () : ML.exp =
              if (List.length exps = 1) then List.hd exps else raise InvalidNumberParameters in
          let verify_double_arg () : ML.exp * ML.exp =
              if (List.length exps = 2) then (List.nth exps 0, List.nth exps 1) else raise InvalidNumberParameters in
          let binop (op: S.primop) : S.exp =
              (* Check argument list of length two *)
              let(e1, e2) = verify_double_arg () in
              (* Return Scish PrimApp *)
              S.PrimApp(op, [(compile_exp e1); (compile_exp e2)]) in
              (match prim with
                  (* Int: Return Scish int *)
                  | ML.Int(i) ->     
                        let _ = verify_zero_arg () in
                        S.Int(i)
                  (* Bool: Return an int of one for true, zero for false *)
                  | ML.Bool(b) ->
                        let _ = verify_zero_arg () in
                        if b then S.Int(1) else S.Int(0)
                  (* Unit: Return an undefined integer value - type check should eliminate any impropper use of unit *)
                  | ML.Unit ->
                        let _ = verify_zero_arg () in
                        S.Int(0)
                  (* Binops - compile sub epxressions and perform operation *) 
                  | ML.Plus -> binop S.Plus
                  | ML.Minus -> binop S.Minus
                  | ML.Times -> binop S.Times
                  | ML.Div -> binop S.Div
                  | ML.Eq -> binop S.Eq
                  | ML.Lt -> binop S.Lt
                  (* Create pair using Scish.Cons *)
                  | ML.Pair ->
                        let (e1, e2) = verify_double_arg () in
                            S.PrimApp(S.Cons, [(compile_exp e1); (compile_exp e2)])
                  | _ -> raise ImplementMe)
(* 
| Plus   (* Add two ints *)
| Minus  (* subtract two ints *)
| Times  (* multiply two ints *)
| Div    (* divide two ints *)
| Eq     (* compare two ints for equality *)
| Lt     (* compare two ints for inequality *)
| Pair   (* create a pair from two values *)
| Fst    (* fetch the 1st component of a pair *)
| Snd    (* fetch the 2nd component of a pair *)
| Nil    (* the empty list *)
| Cons   (* create a list from two values *)
| IsNil  (* determine whether a list is Nil *)
| Hd     (* fetch the head of a list *)
| Tl     (* fetch the tail of a list *)
*)
    (* Build to Scish lambda *)
    | ML.Fn(param, body) -> S.Lambda(param, (compile_exp body))
    (* Apply function to an argument *)
    | ML.App (func, arg) -> S.App((compile_exp func), compile_exp arg)
    (* If control statement - compile to Scish If *)
    | ML.If(e1, e2, e3) -> S.If(compile_exp e1, compile_exp e2, compile_exp e3)
    (* Use Scish sLet, or apply lambda of e2 to e1 *)
    | ML.Let(v, e1, e2) -> S.App(S.Lambda(v, compile_exp e2), compile_exp e1) 


