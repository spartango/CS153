(* TODO:  your job is to map ScishAst expressions to CishAst functions. 
   The file sample_input.scish shows a sample Scish expression and the
   file sample_output.cish shows the output I get from my compiler.
   You will want to do your own test cases...
 *)

exception Unimplemented

type exp = 
  Int of int                    (* integer constants *)
| Var of var                    (* read value in variable *)
| PrimApp of primop * exp list  (* apply primitive operation *)
| Lambda of var * exp           (* an anonymous function *)
| App of exp * exp              (* call a function *)
| If of exp * exp * exp         (* if e1 != 0 then e2 else e3 *)

let rec compile_exp (e:Scish_ast.exp) : Cish_ast.program = 
    let dummy_pos = 0 in
    match e with
        | Int i -> (Cish_ast.Int i, dummy_pos)
        | Var v -> (Cish_ast.Var v, dummy_pos)
        | PrimApp (op, exps) -> raise Unimplemented
        | Lambda (v, e) -> raise Unimplemented
        | App (e1, e2) -> raise Unimplemented
        | If(e1, e2, e3) -> raise Unimplemented
(* Match against Scish_epxressions*)
              (*Int -> Cish int *)
              (*Var -> Cish var *)
              (*PrimApp (op, exps) -> Handle operators *)
