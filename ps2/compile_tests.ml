open Test_framework
open Pretty_print
open Compile

(* TODO: Implement tests *)
let stub = Test("Implemented", (fun () -> false)  )
;;

run_test_set [stub] "Test Stub";;