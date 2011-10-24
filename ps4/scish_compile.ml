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

let int_counter = ref 0
let new_int () = (int_counter := (!int_counter) + 1; !int_counter)
let new_temp() = 
    "temp_var" ^ (string_of_int (new_int ()))
let new_function() = "f" ^ (string_of_int (new_int ()))

let result_var : Cish_ast.exp = (Cish_ast.Var(result_name), stub_pos)

let rec compile_exp_r ( t_expr : Scish_ast.exp ) 
                      ( f_list : func list     ) 
                      ( scope  : var list      )
                      : (func list * var list * stmt) =
    (* Compiles an expression and stores its result in a temporary variable *)
    let compile_store (ex: Scish_ast.exp) (fs: func list) (s: var list) 
            : var * (func list * var list * stmt) =
        let (f_list1, scope1, stmt1) = compile_exp_r ex fs s in
        let temp1 = new_temp () in
        let store_result = cish_stmt_from_str (temp1 ^ " = " ^ result_name ^ ";") in    
            (temp1, (f_list1, scope1, (seqs [stmt1; store_result]))) in

        match t_expr with
                (* Store int in result *)
            | Int(i) -> (f_list, scope, (cish_stmt_from_str (result_name ^ " = " ^ (string_of_int i) ^ ";")))
            | Var(v)            -> (* Scope lookup *)
                  let scope_loc = (scope_index v scope) in
                  let code = lookup_env scope_loc in 
                      (* Env lookup   *)
                      (f_list, scope, code)

            | PrimApp(op, exps) -> 
                  let binop (oper: string) : func list * var list * Cish_ast.stmt =
                      (* Verify that expression list is only length 2 *)
                      let _ = verify_length exps 2 in
                      (* Compile expression and store result in temp1 *)
                      let (temp1, (f_list1, scope1, stmt1)) = compile_store (List.hd exps) f_list scope in
                          (* Compile second expression, storing result in result *)
                      let (f_list2, scope2, stmt2) = compile_exp_r (List.nth exps 1) f_list1 scope1 in
                          (* Perform operation and place result in result *)
                      let end_stmt = cish_stmt_from_str (result_name ^ " = " ^ temp1 ^ oper ^ result_name ^ ";") in
                          (* Concatinate statements using Seq *)
                          (f_list2, scope2, (init_var temp1 (seqs [stmt1; stmt2; end_stmt]))) in 

                  (* Accesses tuple at address ex in memory with an offset in bytes of offset *)
                  let access_tuple (ex: Scish_ast.exp) (fs: func list) (s: var list) (offset: int) =
                      (* Verify exps is one argument long for fst/snd *)
                      let _ = verify_length exps 1 in
                      (* Compiles ex, placing result in result *)
                      let(f_list1, scope1, stmt1) = compile_exp_r ex f_list scope in    
                      (* Places value at (ex+offset) in result *)
                      let access_stmt = cish_stmt_from_str (result_name ^ " = " ^ "*(" ^ result_name ^ "+" ^ (string_of_int offset) ^ ");") in
                          (f_list1, scope1, seqs [stmt1; access_stmt]) in

                      (match op with
                           | Plus   -> binop "+"
                           | Minus -> binop "-"
                           | Times -> binop "*"
                           | Div -> binop "/"
                           | Cons ->
                                 (* Verify that exps is length 2 *)
                                 let _ = verify_length exps 2 in
                                 let tuple_address = new_temp () in
                                     (* Create space to store tuple *)
                                 let init_stmt = cish_stmt_from_str (tuple_address ^ " = malloc(8);") in
                                     (* Compile first expression, placing result in result *) 
                                 let (f_list1, scope1, stmt1) = compile_exp_r (List.hd exps) f_list scope in
                                     (* Store result in first word at the tuple's address *)
                                 let store_stmt1 = cish_stmt_from_str ("*(" ^ tuple_address ^ ") = " ^ result_name ^ ";") in
                                 (* Compile second expression, placing result in result *)
                                 let (f_list2, scope2, stmt2) = compile_exp_r (List.nth exps 1) f_list1 scope1 in
                                 (* Store result in second word at tuple's address *)
                                 let store_stmt2 = cish_stmt_from_str ("*(" ^ tuple_address ^ "+4) = " ^ result_name ^ ";") in
                                 let mv_result   = cish_stmt_from_str (result_name^" = "^tuple_address^";") in
                                     (f_list2, scope2, (init_var tuple_address (seqs [init_stmt; stmt1; store_stmt1; stmt2; store_stmt2; mv_result])))
                                         (* create a pair *)
                           | Fst -> access_tuple (List.hd exps) f_list scope 0 
                           | Snd -> access_tuple (List.hd exps) f_list scope 4
                           | Eq -> binop "=="
                           | Lt -> binop "<")

            | Lambda(v, e1)  -> create_closure v e1 f_list scope
 
            | App(e1, e2)    -> 
                  (* Compile e1 to pointer to function and environment; store in result *)
                  let (f_list1, scope1, stmt1) = compile_exp_r e1 f_list scope in

                  (* Store function address in temp1 and environment in temp2 *)
                  let temp1 = new_temp () in
                  let temp2 = new_temp () in
                  let store_stmt = zip_cish_strs 
                      [(temp1 ^ " = *" ^ result_name ^ ";");
                       (temp2 ^ " = *(" ^ result_name ^ "+4);") ] in

                  (* Compile e2 and store in temp3 *)
                  let (temp3, (f_list2, scope2, stmt2)) = compile_store e2 f_list1 scope1 in

                  (* Place updated environment in result and call function *)
                  let malloc_new_env = zip_cish_strs
                      (* Malloc space *)
                      [ result_name ^ " = malloc(8);";
                        (* Store value of e2 in first word *)
                        "*" ^ result_name ^ " = " ^ temp3 ^ ";";
                        (* Store previous environment in second word *)
                        "*(" ^ result_name ^ "+4) = " ^ temp2 ^ ";"; 
                        (* Call function with environment, returning result to result *)
                        result_name ^ " = " ^ temp1 ^ "(" ^ result_name ^ ");" ] in
                      (* Return all, initializing variables *)
                      (f_list2, scope2, (
                           (init_var temp1 (init_var temp2 (init_var temp3 
                                                                (seqs [stmt1; store_stmt; stmt2; malloc_new_env]))))))

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
  (* Wrap variable declarations and put return in body *)
  let f_body1 = init_result (return_result f_body) in
  (* Generate Function *)
  let function_name = (new_function ()) in
  let new_func  = Fn( { name = function_name; 
                        args = ["env"]; 
                        body = f_body1;
                        pos  = stub_pos;
                      } 
                  ) in
  (* Allocate Space    *)
  (* Put pointers      *)
  let code = seqs (List.map cish_stmt_from_str 
                       [ "result = malloc(8);";
                         "*result = " ^ function_name ^ ";";
                         "*(result+4) = env;"])
  in
  ( (new_f_list @ [new_func]),
    scope, 
    code )



(* Initialize an environment    *)
(* Env starts as a null pointer *)
let init (t_expr : Scish_ast.exp) : (func list * stmt) =
  let (fns, _, code) = compile_exp_r t_expr [] []     in
  (* Add return to main *)
  let new_code       = return_result code             in
  let new_code       = init_result   new_code         in
  let new_code       = init_env      new_code         in
  (fns, new_code)

(* Create a main function       *)
let compile_exp (e:Scish_ast.exp) : Cish_ast.program =
  let (functions, main_body) = (init e) in
    (functions @ [Fn( { name = "main"; 
            args = []; 
            body = main_body; 
            pos  = 0;
       } )]
     )
