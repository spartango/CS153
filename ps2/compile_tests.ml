open Test_framework
open Pretty_print
open Compile
open Optimize
open Ast
open Mips

module IList = RevList(struct type element = int end)

(* Utility function tests *)
let revapp_test = 
    let (<@) a b = IList.app_list a b in
    let test = fun () ->
	let init_list   = IList.rev_list [1;2;3]    in
	let target_list = [4;5]                  in
	let result = IList.to_list (init_list <@ target_list) in
	    result = [1;2;3;4;5]
    in 
	Test("Revapp Test", test)
;;

(* Tests for collecting variables *)
let collect_assign_test = 
	let test = fun () -> 
		let prog = (Ast.Exp(
						(Ast.Assign("y", 
								    (Int(1), 0)
									), 0)
								), 0)
		in 
		let _ = reset () in
		let _ = (collect_vars prog) in
		(find_var "Vy")
	in
	Test("Assignment Var Collect", test)
;;

let collect_rec_assign_test = 
	let test = fun () -> 
		let prog = (Ast.Exp(
						(Ast.Assign("y", 
								  ((Ast.Assign("x", 
									  (Ast.Int(2), 0))), 0)
									), 0)
								), 0)
		in 
		let _ = reset () in
		let _ = (collect_vars prog) in
		find_var "Vx" 
	in
	Test("Nested Assign Var Collect", test)
;;

let collect_exp_assign_test = 
	let test = fun () -> 
		let prog = (Ast.Exp(
						(Ast.Binop((Int(5), 0), Ast.Plus, 
								  ((Ast.Assign("x", 
									  (Ast.Int(2), 0))), 0)
									), 0)
								), 0)
		in 
		let _ = reset () in
		let _ = (collect_vars prog) in
		find_var "Vx" 
	in
	Test("Expression Assign Var Collect", test)
;;

(* Tests for compiling expressions *)

let compile_assign_test = 
	let test = fun () -> 
		let prog = (Ast.Exp(
						(Ast.Assign("y", 
								    (Int(1), 0)
									), 0)
								), 0)
		in 
		let _ = reset () in
		let result = (compile_stmt prog) in
		let success = 
			(result = 
			[ Li(R2, 1l); La(R3, "Vy"); Sw(R2,R3, Int32.zero) ])
		in
		success(*, (String.concat "" (code_to_string result)))*)
	in
	Test("Compile Var Assign", test)
;;

(* Tests for compiling specific statements *)
let compile_if_assign_test = 
	let if_cond     = (Int(0), 0) in
	let then_assign = (Ast.Exp(
						(Ast.Assign("y", 
								    (Int(1), 0)
									), 0)
								), 0)
	in
	let else_c      = (skip, 0) in
	let if_stmt = (Ast.If(
						  if_cond,
						  then_assign,
						  else_c), 0)
	in 
	let test = fun () -> 
		let compiled = (compile_stmt if_stmt) in
		let success  = 
			(compiled =
			[ Li(R2, 0l); 
			  Beq(R2, R0, "L1");
			  Li(R2, 1l); 
			  La(R3, "Vy"); 
			  Sw(R2,R3, 0l);
			  J("L2");
			  Label("L1");
			  Li(R2, 0l); 
			  Label("L2"); ]
			)
		in success
	in 
	Test("Compile If-Assign hybrid", test) 
;;

let jump_thread_test = 
	let code = 
		[ 
			Li(R2, 0l);
			J("L2");
			Label("L1");
			J("L3");
			Li(R2, 2l);
			Label("L2");
			J("L1");
			Li(R2, 4l);
			Label("L3");
			Li(R2, 8l);
		]
	in 
	let test = fun () -> 
		let thin_code = thread_jumps code in
		let success = 
			(thin_code = 
				[ 
					Li(R2, 0l);
					J("L3");
					Label("L1");
					J("L3");
					Li(R2, 2l);
					Label("L2");
					J("L3");
					Li(R2, 4l);
					Label("L3");
					Li(R2, 8l);
				])
		in 
		success
	in 
	Test("Thread Jumps Test", test)
;;

let constant_folding_test = 
	let program = (Ast.Exp(
						(Ast.Binop( 
								    (Int(1), 0), 
								    Ast.Plus, 
								    (Int(1), 0)
									), 0)
								), 0)
		
	in
	let test = fun () -> 
		let folded = (constant_fold program) in
		let success = 
			(folded =
				(Ast.Exp(
						(Ast.Int(2), 0)
								), 0)
			)
		in 
		success
	in
	Test("Constant Folding Test", test)
;;
			

run_test_set [ revapp_test;  ] "Utility Tests";;

run_test_set [ collect_assign_test; 
			   collect_rec_assign_test;
			   collect_exp_assign_test; ] 
			 "Collect Var Tests";;

run_test_set [ compile_assign_test ] "Compile Expression Tests";;

run_test_set [ compile_if_assign_test ] "Compile Statment Tests";;

run_test_set [ jump_thread_test;
			   constant_folding_test; ] "Optimization Tests";;
