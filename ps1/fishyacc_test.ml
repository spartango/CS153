open Test_framework
open Fishyacc_helpers
open Parsing
open Str
open Eval

(* Assumes all test files in test *)
let mk_file_test (file: string) (expected: int) =
    mk_verbose_expect_test (fun () -> (eval (parse_code ("test/" ^ file)))) expected string_of_int
        ("Testing program in " ^ file)

let mk_parse_test (file: string) =
    let label = "Parsing " ^ file in
       
            Verbose_Test(label, (fun()->
                                     try    
                                         let rslt = eval (parse_code file) in
                                             (true, "Parsed with answer " ^ string_of_int rslt)
                                     with
                                             Parsing.Parse_error -> 
                                                     (false, "Failed parse")))
   
let parse_test_files_list =
    List.fold_left (fun tests case -> 
                   if (case = ".DS_Store")
                   then tests
                   else (mk_parse_test ("test/" ^ case))::tests)
         [] (Array.to_list (Sys.readdir "test"))

let test_silly =
    (mk_file_test "silly.fish" 8);;
    
run_test_set [test_silly] "Lex/Yacc Parser";;
run_test_set parse_test_files_list "Lex/Yacc Parse of Test Files";;
