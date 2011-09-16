open Test_framework

let stub = Test("Implemented", (fun () -> false)  );;
    
run_test_set [stub;] "Test Stub"