open Mips_sim
open Mips_ast

let run_verbose_test (test : unit -> bool * string) (name : string) : string  = 
    let result = (test ()) in 
    match result with
    | (true,  message) -> "[  PASSED  ] "^name^": "^message^"\n"
    | (false, message) -> "[  FAILED  ] "^name^": "^message^"\n"

let run_test  (test : unit -> bool) (name : string ) : string = 
    let result = (test ()) in 
    if result then "[  PASSED  ] "^name^"\n" 
              else "[  FAILED  ] "^name^"\n"

let test_verbose_inst_translate = fun () -> 
    let test_inst = Add(R4, R5, R6)      in 
    let binary = 0x00a62020l             in
    let result = (inst_to_bin test_inst) in
    ((result = binary), (Int32.to_string result)^" vs "^(Int32.to_string binary))

let test_inst_translate = fun () -> 
    let test_inst = Add(R4, R5, R6) in 
    let binary = 0x00a62020l in
    (inst_to_bin test_inst) = binary

let _ = print_string "[==========] Running Tests\n"
let _ = print_string (run_verbose_test test_verbose_inst_translate "Translate")
let _ = print_string "[==========] Tests Complete\n";;