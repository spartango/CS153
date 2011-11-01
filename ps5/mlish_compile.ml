module ML = Mlish_ast
module S = Scish_ast

exception ImplementMe
exception InvalidNumberParameters

(* Lists: Each Cons cell, implemented as a pair, is wrapped within inside a pair;
the first element is the cons cell. The second element states whether that cell is
nil or not. Wastes memory, but clean implementation. *)

let is_nil = S.Int(1)
let not_nil = S.Int(0)

(* Value inside a nil wrapper - can be anything *)
let nil_content = S.Int(0)

let nil_cell = S.PrimApp(S.Cons, [nil_content; is_nil])

(* Unit *)
let val_unit = 0
let unit = S.Int(val_unit)

let rec compile_exp_r ((e,_):ML.exp) : S.exp = 
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
              S.PrimApp(op, [(compile_exp_r e1); (compile_exp_r e2)]) in
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
                        S.Var("Unit")
                  (* Binops - compile sub epxressions and perform operation *) 
                  | ML.Plus -> binop S.Plus
                  | ML.Minus -> binop S.Minus
                  | ML.Times -> binop S.Times
                  | ML.Div -> binop S.Div
                  | ML.Eq -> binop S.Eq
                  | ML.Lt -> binop S.Lt
                  (* Create pair using Scish.Cons *)
                  | ML.Pair -> binop S.Cons
                  | ML.Fst ->
                        let e = verify_single_arg () in
                            S.PrimApp(S.Fst, [compile_exp_r e])
                  | ML.Snd ->
                        let e = verify_single_arg () in
                            S.PrimApp(S.Snd, [compile_exp_r e])
                  (* Create list using Cons *)
                  | ML.Cons -> 
                        let (e1, e2) = verify_double_arg () in
                        (* Create the cons cell of the hd (e1) and tail (e2) *)
                        let cell = S.PrimApp(S.Cons, [compile_exp_r e1;compile_exp_r e2]) in
                        (* Create wrapper cell containing information as to whether it is nill *)
                            S.PrimApp(S.Cons, [cell; not_nil])
                  | ML.Hd -> 
                        let e = verify_single_arg () in
                            (* Gets cons cell out of wrapper and returns first element *)
                            S.PrimApp(S.Fst, [S.PrimApp(S.Fst, [compile_exp_r e])])
                  | ML.Tl ->
                        let e = verify_single_arg () in
                            (* Gets cons cell out of wrapper and returns second element *)
                            S.PrimApp(S.Snd, [S.PrimApp(S.Fst, [compile_exp_r e])])
                  | ML.Nil -> 
                        let _ = verify_zero_arg () in 
                        (* Creates a wrapper cell with no cons cell and but information saying is_nil *)
                        nil_cell
                  | ML.IsNil -> 
                        let e1 = verify_single_arg () in
                            (* Return the value of the is_nil part of the cons pair cell *)
                            S.PrimApp(S.Snd, [compile_exp_r e1])
                            
              )

    (* Build to Scish lambda *)
    | ML.Fn(param, body) -> S.Lambda(param, (compile_exp_r body))
    (* Apply function to an argument *)
    | ML.App (func, arg) -> S.App((compile_exp_r func), compile_exp_r arg)
    (* If control statement - compile to Scish If *)
    | ML.If(e1, e2, e3) -> S.If(compile_exp_r e1, compile_exp_r e2, compile_exp_r e3)
    (* Use Scish sLet, or apply lambda of e2 to e1 *)
    | ML.Let(v, e1, e2) -> S.App(S.Lambda(v, compile_exp_r e2), compile_exp_r e1) 

let compile_exp (e: Mlish_ast.exp) : S.exp =
    (* Wrapper that defines Unit *)
    S.sLet "Unit" unit (compile_exp_r e)


