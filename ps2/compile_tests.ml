open Test_framework
open Pretty_print
open Compile
open Ast

(* Tests for collecting variables *)
let collect_assign_test = 
	let test = fun () -> 
		let prog = (Ast.Exp(
						(Ast.Assign("y", 
								    (Int(1), 0)
									), 0)
								), 0)
		in 
		let _ = (collect_vars prog) in
		VarSet.exists (fun x -> x = "y") !variables 
	in
	Test("Assignment Var Collect", test)
;;

let collect_rec_assign_test = 
	let test = fun () -> 
		let variables : VarSet.t ref = ref (VarSet.empty) in
		let prog = (Ast.Exp(
						(Ast.Assign("y", 
								  ((Ast.Assign("x", 
									  (Ast.Int(2), 0))), 0)
									), 0)
								), 0)
		in 
		let _ = (collect_vars prog) in
		VarSet.exists (fun x -> x = "y") !variables 
	in
	Test("Nested Assign Var Collect", test)
;;

(* Tests for compiling expressions *)

(* Tests for compiling specific statements *)

(* Tests for compiling compound statements *)


(* TODO: Implement tests *)
let stub = Test("Implemented", (fun () -> false)  )
;;

run_test_set [ collect_assign_test; 
			   collect_rec_assign_test ] 
			 "Collect Var Tests";;

run_test_set [stub] "Compile Expression Tests";;

run_test_set [stub] "Compile Statment Tests";;

run_test_set [stub] "Compile Block Statement Tests";;


