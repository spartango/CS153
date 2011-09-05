open Mips_sim
open Mips_ast
open Test_framework

let test_verbose_inst_translate = fun () -> 
    let test_inst = Add(R4, R5, R6)      in 
    let binary = 0x00a62020l             in
    let result = (inst_to_bin test_inst) in
         ((result = binary), (Int32.to_string result)^" vs "^(Int32.to_string binary))

let test_inst_translate = fun () -> 
    let test_inst = Add(R4, R5, R6) in 
    let binary = 0x00a62020l in
         (inst_to_bin test_inst) = binary

let test_update_mem = fun () ->
    let init_state = {m = empty_mem; pc = 0l; r = empty_rf} in
    let test_inst  = Add(R4, R5, R6)                        in
    let new_state  = (inst_update_mem test_inst init_state) in 
    (* We're looking for 0x00A62020 split into 4 bytes *)
        (  ((mem_lookup 0l new_state.m) = (Byte.mk_byte 0x00l)) 
        && ((mem_lookup 1l new_state.m) = (Byte.mk_byte 0xa6l))
        && ((mem_lookup 2l new_state.m) = (Byte.mk_byte 0x20l))
        && ((mem_lookup 3l new_state.m) = (Byte.mk_byte 0x20l)) )
        

let test_assemble_prog = fun () -> 
    let test_program    = [ Add(R4, R5, R6);  Add(R4, R5, R6) ] in 
    let new_state = (assem test_program) in 
    (* We're looking for 0x00A62020 split into 4 bytes *)
        (  ((mem_lookup 0l new_state.m) = (Byte.mk_byte 0x00l)) 
        && ((mem_lookup 1l new_state.m) = (Byte.mk_byte 0xa6l))
        && ((mem_lookup 2l new_state.m) = (Byte.mk_byte 0x20l))
        && ((mem_lookup 3l new_state.m) = (Byte.mk_byte 0x20l))
        && ((mem_lookup 4l new_state.m) = (Byte.mk_byte 0x00l))
        && ((mem_lookup 5l new_state.m) = (Byte.mk_byte 0xa6l))
        && ((mem_lookup 6l new_state.m) = (Byte.mk_byte 0x20l))
        && ((mem_lookup 7l new_state.m) = (Byte.mk_byte 0x20l)) 
        && (new_state.pc = 0l) )
        

(run_tests [ Test(test_inst_translate,         "Translate"); 
            Test(run_test test_update_mem,    "Update Memory"); 
            Test(run_test test_assemble_prog, "Assembled Program") ])
