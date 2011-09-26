open Test_framework
open Fishyacc_helpers
open Parsing
open Str
open Eval
open Ast

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
  
let file_answers =
    [ ("01cexpr_01add.fish", 16);
      ("01cexpr_02sub.fish", 8);
      ("03stmt_05if.fish", 7);
      ("03stmt_06for.fish", 715);
      ("09all_02fibo.fish", 1597);
      ("09all_01adder.fish", 5);
      ("04opt_02cfoldif.fish", 5);
      ("04opt_01cfoldif.fish", 3)]

let tests_file_answers =
    List.map (fun case -> let(file,answer) = case in mk_file_test file answer) file_answers;;


run_test_set parse_test_files_list "Lex/Yacc Parse of Test Files";;
run_test_set tests_file_answers "Lex/Yacc Correct Eval of Test Files";;
