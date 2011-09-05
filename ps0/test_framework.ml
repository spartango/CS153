type test = Verbose_Test of string * (unit -> bool * string) | Test of string * (unit -> bool)

let run_test  (t_test : test) : string = 
    match t_test with 
        | Test(name, exec) ->
		    let result = (exec ()) in 
		    if result then "[  PASSED  ] "^name^"\n" 
		              else "[  FAILED  ] "^name^"\n"
        | Verbose_Test(name, exec) -> 
            let result = (exec ()) in 
			match result with
			| (true,  message) -> "[  PASSED  ] "^name^": "^message^"\n"
			| (false, message) -> "[  FAILED  ] "^name^": "^message^"\n"
			     
            
let run_tests ( tests : test list) : unit = 
    let _ = print_string "[==========] Running Tests\n"  in
    let rec run_tests_h ( tests : test list ) : unit = 
        match tests with 
            | []             -> () (* done *)
            | t_test :: rest -> 
                let _ = print_string (run_test t_test) in 
                (run_tests_h rest)
    in
    let _ = run_tests_h tests in 
    let _ = print_string "[==========] Tests Complete\n" in ()
    
