open Test_framework
open Pretty_print
open Compile
open Ast
open Mips

(* Utility function tests *)
let revapp_test = 
	let test = fun () ->
		let init_list   = [3; 2; 1]               in
		let target_list = [4; 5]                  in
		let result = revapp init_list target_list in
		result = [5;4;3;2;1]
	in 
	Test("Revapp Test", test)
;;

let rev_test = 
	let test = fun () -> 
		let init_list = [3; 2; 1]     in
		let result    = rev init_list in
		result = [1; 2; 3] 
	in
	Test("Rev Test", test)
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

(* Tests for compiling compound statements *)


(* TODO: Implement tests *)
let stub = Test("Implemented", (fun () -> false)  )
;;

run_test_set [ revapp_test;
			   rev_test    ] "Utility Tests";;

run_test_set [ collect_assign_test; 
			   collect_rec_assign_test;
			   collect_exp_assign_test; ] 
			 "Collect Var Tests";;

run_test_set [compile_assign_test] "Compile Expression Tests";;

run_test_set [stub] "Compile Statment Tests";;

run_test_set [stub] "Compile Block Statement Tests";;


