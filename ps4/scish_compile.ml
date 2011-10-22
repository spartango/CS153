(* TODO:  your job is to map ScishAst expressions to CishAst functions. 
   The file sample_input.scish shows a sample Scish expression and the
   file sample_output.cish shows the output I get from my compiler.
   You will want to do your own test cases...
 *)

open Environment 
open Utility
open Cish_ast
open Scish_ast

exception Unimplemented

let label_counter = ref 0
let new_int()   = (label_counter := (!label_counter) + 1; !label_counter)
let new_temp() = 
    "temp_var" ^ (string_of_int (new_int()))
let new_function() = "f" ^ (string_of_int (new_int()))

let result_name = "result";;
let result_var : Cish_ast.exp = (Cish_ast.Var(result_name), stub_pos)

let rec compile_exp_r ( t_expr : Scish_ast.exp ) 
                      ( f_list : func list     ) 
                      ( scope  : var list      )
                      : (func list * var list * stmt) =
  match t_expr with
  (* Store int in result *)
  | Int(i) -> (f_list, scope, (cish_stmt_from_str ("result = " ^ (string_of_int i))))
  | Var(v)            -> (* Scope lookup *)
                          let scope_loc = (scope_index v scope) in
                          let code = lookup_env scope_loc in 
                         (* Env lookup   *)
                          (f_list, scope, code)
  | PrimApp(op, exps) -> 
        let rec seqs (stmts: Cish_ast.Stmt list) : Cish_ast.stmt =
            match stmts with
                | [] -> Cish_ast.skip
                | hd::[] -> hd
                | hd::tl -> Cish_ast.Seq(hd, seqs tl) in
        (* Compiles an expression and stores its result in a temporary variable *)
        let compile_store (ex: Scish_ast.exp) (fs: func list) (s: var list) 
                : var * (func list * var list * stmt) =
            let (f_list1, scope1, stmt1) = compile_exp_r ex fs s in
            let temp = new_temp () in
            let store_result = cish_stmt_from_str (temp1 ^ " = " ^ result_name) in    
                (temp1, (f_list1, scope1, (seqs [stmt1; store_result]))) in
        let binop (oper: string) : func list * var list * Cish_ast.stmt =
            (* Compile expression and store result in temp1 *)
            let (temp1, (f_list1, scope1, stmt1)) = compile_store (List.hd exps) f_list scope in
            (* Compile second expression, storing result in result *)
            let (f_list2, scope2, stmt2) = compile_exp_r (List.nth exps 1) f_list1 scope1 in
            (* Perform operation and place result in result *)
            let end_stmt = cish_stmt_from_str (result_name ^ " = " ^ temp1 ^ oper ^ result_name) in
                (* Concatinate statements using Seq *)
                (f_list2, scope2, seqs [stmt1; stmt2; end_stmt]) in      
            match op with
                | Plus   -> binop "+"
                | Minus -> binop "-"
                | Times -> binop "*"
                | Div -> binop "/"
                | Cons ->
                      let tuple_address = new_temp () in
                      (* Create space to store tuple *)
                      let init_stmt = tuple_address ^" = malloc(8);" in
                      (* Compile first expression, placing result in result *) 
                      let (f_list1, scope1, stmt1) = compile_exp_r (List.hd exps) f_list scope in
                      let store_stmt1 = "*(" ^ tuple_address ^ ") = " ^ result_name ^ ";" in
                      (* Compile second expression, placing result in result *)
                      let (f_list2, scope2, stmt2) = compile_exp_r (List.nth exps 1) f_list1 scope1 in
                      let (temp1, ) = compile_store (List.hd exps) f_list scope in
                      let (temp2, (f_list2, scope2, stmt2)) = compile_store (List.nth exps 1) f_list1 scope1 in
                      let store1 = "



 (* create a pair *)
| Fst    (* fetch the 1st component of a pair *)
| Snd    (* fetch the 2nd component of a pair *)
| Eq     (* compare two ints for equality *)
| Lt     (* compare two ints for inequality *)
  | Lambda(v, t_exp)  -> create_closure v t_expr f_list scope 
  | App(e1, e2)       -> (* Compile e2 *)
                         (* Push result on to stack *)
                         (* Call *)
                         (* Pop *)
                         raise Unimplemented (* TODO: call           *)
  | If(e1, e2, e3)    -> let (new_f_list, _, e1_code) = 
                            compile_exp_r e1 f_list scope in
                         let (new_f_list, _, e2_code) = 
                            compile_exp_r e2 new_f_list scope in
                         let (new_f_list, _, e3_code ) = 
                            compile_exp_r e3 new_f_list scope in
                         let if_s = ( Cish_ast.If(
                                        (Cish_ast.Var(result_name), stub_pos),
                                        e2_code, 
                                        e3_code), stub_pos )
                          in
                          let code = ( Seq(e1_code, if_s), stub_pos) in
                          (new_f_list, scope, code) 

and create_closure (arg : string) 
                   (body : exp) 
                   (f_list : func list) 
                   (scope : var list) 
                   : (func list * var list * stmt) =
  (* Push scope        *)
  let new_scope = push_scope arg scope in
  let (new_f_list, _, f_body) = (compile_exp_r body f_list new_scope) in
  (* Generate Function *)
  let function_name = (new_function ()) in
  let new_func  = Fn( { name = function_name; 
                        args = ["env"]; 
                        body = f_body;
                        pos  = stub_pos;
                      } 
                  ) in
  (* Allocate Space    *)
  (* Put pointers      *)
  let code = cish_stmt_from_str ("result = malloc(8); "
                                 ^"*result = "^function_name^"; "
                                 ^"*(result+4) = env;" )
  in
  ( ([new_func] @ f_list),
    scope, 
    code )

let init_result (code : stmt) : stmt =
  (Let(result_name, (null, stub_pos), code), stub_pos)

(* Initialize an environment    *)
(* Env starts as a null pointer *)
let init (t_expr : Scish_ast.exp) : (func list * stmt) =
  let (fns, _, code) = compile_exp_r t_expr [] []     in
  let new_code       = init_result   code             in
  let new_code       = init_env      new_code         in
  (fns, new_code)

(* Create a main function       *)
let rec compile_exp (e:Scish_ast.exp) : Cish_ast.program =
  let (functions, main_body) = (init e) in
    ([Fn( { name = "main"; 
            args = []; 
            body = main_body; 
            pos  = 0;
       } )]
     @ functions)
