open Pretty_print

type test = Verbose_Test of string * (unit -> bool * string) | Test of string * (unit -> bool)

let mk_expect_test (f : unit -> 'a) (expected : 'a) (name : string) : test = 
    let t_test = fun () -> (f ()) = expected in
    Test(name, t_test)

let mk_verbose_expect_test (f : unit -> 'a) (expected : 'a) (to_string : 'a -> string) (name : string) : test = 
    let t_test = fun () -> 
        let result  = (f ())              in 
        let pass    = (result = expected) in
        let message = 
            if pass 
                then "Got expected result -> "^(format_string (to_string result) Bright Green) 
                else "Result doesn't match expected -> "^(format_string (to_string result) Bright Red)
                     ^" vs "^(format_string (to_string expected) Bright Green)
        in (pass, message)
    in
    Verbose_Test(name, t_test)

let run_test  (t_test : test) : (bool * string) = 
    match t_test with 
        | Test(name, exec) ->
		    let result = (exec ()) in 
            (result,  
            (if result then (format_string "[  PASSED  ] " Bright Green) 
                       else (format_string "[  FAILED  ] " Bright Red))
                ^name^"\n")
        | Verbose_Test(name, exec) -> 
            let (result, message) = (exec ()) in 
			(result,  
            (if result then (format_string "[  PASSED  ] " Bright Green) 
                       else (format_string "[  FAILED  ] " Bright Red))
                ^name^": "^message^"\n")
			     
            
let run_test_set (tests : test list) (set_name : string) : unit = 
    let _ = print_string ((format_string "\n[==========] " Bright Cyan)^"Running "^set_name^"\n")  in
    let rec run_tests_h ( tests : test list ) ( pass : bool ) : bool = 
        match tests with 
            | []             -> pass
            | t_test :: rest -> 
                let (result, message) = (run_test t_test) in 
                let _ = print_string message in 
                (run_tests_h rest (pass && result))
    in
    let pass = run_tests_h tests true in 
    print_string ((format_string "[==========] " Bright Cyan)^"Tests "
                   ^(if pass then "Passed" else "Failed")^"\n\n")

let run_tests (tests : test list) : unit = 
    (run_test_set tests "Tests") 
            
let run_expect_test  (f : unit -> 'a) (expected : 'a) (name : string) =
    run_test ( mk_expect_test f expected name )

let run_verbose_expect_test (f : unit -> 'a) (expected : 'a) (to_string : 'a -> string) (name : string) = 
    run_test ( mk_verbose_expect_test f expected to_string name )