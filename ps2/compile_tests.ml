open Test_framework
open Pretty_print
open Compile

(* Tests for collecting variables *)

(* Tests for compiling expressions *)

(* Tests for compiling specific statements *)

(* Tests for compiling compound statements *)


(* TODO: Implement tests *)
let stub = Test("Implemented", (fun () -> false)  )
;;

run_test_set [stub] "Collect Var Tests";;

run_test_set [stub] "Compile Expression Tests";;

run_test_set [stub] "Compile Statment Tests";;

run_test_set [stub] "Compile Block Statement Tests";;


