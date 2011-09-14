open Mips_sim
open Mips_ast
open Test_framework
open Binary_ops
open Pretty_print
open Byte

(* add $4 $5 $6 -> 0x00a62020 *)
(* ori $6 $5 34 -> 0x34a60022 *)

(* Binary_ops tests *)
let mk_masker_test (length: int) (offset: int) (expected: int32) =
    mk_verbose_expect_test (fun () -> masker length offset) 
                           expected 
                           Int32.to_string
                           ("Bitmask: length " ^ (string_of_int length) ^
                           " and offset " ^ (string_of_int offset))

(* Masker Tests *)
let masker_test1 = mk_masker_test 1 0 Int32.min_int
let masker_test2 = mk_masker_test 31 1 Int32.max_int
let masker_test3 = mk_masker_test 16 16 0x0000FFFFl
let masker_test4 = mk_masker_test 8 12 0x000FF000l
let masker_test5 = mk_masker_test 1 31 0x00000001l
let masker_test6 = mk_masker_test 17 0 0xFFFF8000l

let masker_tests = [ masker_test1;
                     masker_test2;
                     masker_test3;
                     masker_test4;
                     masker_test5;
                     masker_test6 ]

(* Int16 to 32 tests *)
let mk_int32_to_int16_test (i32: int32) (i16 : int32) =
    mk_verbose_expect_test (fun () -> int32_to_int16 i32)
                           i16
                           Int32.to_string
                           ("Translate " ^ (Int32.to_string i32) ^ 
                                " from 32 to 16 bits")

let int32_to_int16_test1 = mk_int32_to_int16_test 0x00000001l 0x0001l
let int32_to_int16_test2 = mk_int32_to_int16_test (-0x00000001l) 0xFFFFl
let int32_to_int16_test3 = mk_int32_to_int16_test Int32.max_int 0x7FFFl
let int32_to_int16_test4 = mk_int32_to_int16_test Int32.min_int 0x8001l

let int32_to_int16_tests = [ int32_to_int16_test1;
                             int32_to_int16_test2;
                             int32_to_int16_test3 ]

let mk_int16_to_int32_test (i16 : int32) (i32: int32) =
    mk_verbose_expect_test (fun () -> int16_to_int32 i16)
                           i32
                           Int32.to_string
                           ("Translate " ^ (Int32.to_string i32) ^ 
                                " from 16 to 32 bits")

let int16_to_int32_test1 = 
    mk_int16_to_int32_test 0x0001l 0x00000001l

let int16_to_int32_test2 = 
    mk_int16_to_int32_test 0xFFFFl (-0x00000001l)

let int16_to_int32_test3 = 
    mk_int16_to_int32_test (int32_to_int16 (-0x2Fl)) (-0x0000002Fl)

let int16_to_int32_test4 = 
    mk_int16_to_int32_test 0x002Fl 0x00000002Fl

let int16_to_int32_test5 = 
    mk_int16_to_int32_test 0x7FFFl 0x00007FFFl

let int16_to_int32_test6 = 
    mk_int16_to_int32_test 0x8001l (-0x00007FFFl)

let int16_to_int32_tests = [int16_to_int32_test1;
                            int16_to_int32_test2;
                            int16_to_int32_test3;
                            int16_to_int32_test4;
                            int16_to_int32_test5;
                            int16_to_int32_test6 ]

(* Assembly Tests for each instruction *)
let mk_inst_to_bin_test (test_inst : inst) (expected : int32) =
    (mk_verbose_expect_test (fun () -> (inst_to_bin test_inst)) 
                           expected 
                           Int32.to_string
                           ("Translate "^(inst_to_string test_inst)) )

let test_add_translate = 
    (mk_inst_to_bin_test (Add(R4, R5, R6)) 0x00a62020l)

let test_ori_translate = 
    (mk_inst_to_bin_test (Ori(R6, R5, 34l)) 0x34a60022l)

let test_lui_translate = 
    (mk_inst_to_bin_test (Lui(R11, 255l)) 0x3c0b00ffl)

let test_lw_translate = 
    (mk_inst_to_bin_test (Lw(R10, R6, 4l)) 0x8cca0004l)

let test_sw_translate = 
    (mk_inst_to_bin_test (Sw(R15, R9, 8l)) 0xad2f0008l)

let test_beq_neg_translate = 
    (mk_inst_to_bin_test (Beq(R18, R6, -28l)) 0x1246FFE4l)

let test_beq_translate = 
    (mk_inst_to_bin_test (Beq(R18, R6, 4l)) 0x12460004l)

let test_jr_translate = 
    (mk_inst_to_bin_test (Jr(R21)) 0x02a00008l)

let test_jal_translate = 
    (mk_inst_to_bin_test (Jal(0x00400024l)) 0x0c100009l)

let assemble_inst_tests = [ test_add_translate; 
                            test_ori_translate; 
                            test_lui_translate;
                            test_lw_translate;
                            test_sw_translate;
                            test_beq_translate;
                            test_beq_neg_translate;
                            test_jr_translate;
                            test_jal_translate  ]

(*Disassembly tests for each instruction*)
let mk_disassem_test (i: inst) (bin: int32) = 
    mk_verbose_expect_test (fun () -> disassem bin) i inst_to_string
                           ("Disassemble " ^ (inst_to_string i))

let test_add_disassem =
    mk_disassem_test (Add(R4, R5, R6)) 0x00a62020l 

let test_ori_disassem =
    mk_disassem_test  (Ori(R6, R5, 34l)) 0x34a60022l

let test_lui_disassem = 
    (mk_disassem_test (Lui(R11, 255l)) 0x3c0b00ffl)

let test_lw_disassem = 
    (mk_disassem_test (Lw(R10, R6, 4l)) 0x8cca0004l)

let test_sw_disassem = 
    (mk_disassem_test (Sw(R15, R9, 8l)) 0xad2f0008l)

let test_beq_neg_disassem = 
    (mk_disassem_test (Beq(R18, R6, -28l)) 0x1246FFE4l)

let test_beq_disassem = 
    (mk_disassem_test (Beq(R18, R6, 4l))  0x12460004l)

let test_jr_disassem = 
    (mk_disassem_test (Jr(R21)) 0x02a00008l)

let test_jal_disassem = 
    (mk_disassem_test (Jal(0x00400024l)) 0x0c100009l)

let disassemble_inst_tests = [ test_add_disassem; 
                               test_ori_disassem; 
                               test_lui_disassem;
                               test_lw_disassem;
                               test_sw_disassem;
                               test_beq_disassem;
                               test_beq_neg_disassem;
                               test_jr_disassem;
                               test_jal_disassem  ]

(* Exec Tests *) 
let mk_exec_test (t_inst : inst) (init_state : state) (end_state : state) = 
    Verbose_Test(
        ("Exec "^(inst_to_string t_inst)),
        (fun () ->
            let new_state = (exec t_inst init_state)                in
            let mem_diff  = (compare_mem new_state.m  end_state.m)  in
            let reg_diff  = (compare_rf  new_state.r  end_state.r)  in 
            let pc_diff   = (Int32.sub   new_state.pc end_state.pc) in
            if  (pc_diff = 0l && mem_diff = "" && reg_diff = "") 
                then (true,  "Expected end state reached")
                else (false, "Unexpected end state, differs: "^
                             (format_string 
	                             ("Memory=["^mem_diff
	                             ^"]; Registers=["^reg_diff
	                             ^"]; PC offset by "^(Int32.to_string pc_diff)) 
                              Bright Yellow) )
         ) )
        
let test_exec_add = 
    let rf_2 = (rf_update_many [(6, 1l); (5, 2l)] empty_rf) in
    let rf_f = (rf_update (reg2ind R4) 3l rf_2)             in
    let init_state  = { r = rf_2; m = empty_mem; pc = 0l }  in 
    let final_state = { r = rf_f; m = empty_mem; pc = 4l }  in
    (mk_exec_test (Add(R4, R5, R6)) init_state final_state) 

let test_exec_ori = 
    let rf_i = (rf_update 6 5l empty_rf) in
    let rf_f = (rf_update (reg2ind R4) 7l rf_i)             in
    let init_state  = { r = rf_i; m = empty_mem; pc = 0l }  in 
    let final_state = { r = rf_f; m = empty_mem; pc = 4l }  in
    (mk_exec_test (Ori(R4, R6, 3l)) init_state final_state) 

let test_exec_lui = 
    let rf_i = empty_rf in
    let rf_f = (rf_update (reg2ind R4) 0x00090000l rf_i)    in
    let init_state  = { r = rf_i; m = empty_mem; pc = 0l }  in 
    let final_state = { r = rf_f; m = empty_mem; pc = 4l }  in
    (mk_exec_test (Lui(R4, 9l)) init_state final_state) 
    
(* Branch forward when equal *)    
let test_exec_beq1=
    let rf_i = rf_update_many [(8, 42l); (9, 42l)] empty_rf in
    let init_state  = { r = rf_i; m = empty_mem; pc = 0l } in
    let final_state = { r = rf_i; m = empty_mem; pc = 8l } in
    (mk_exec_test (Beq(R8, R9, 2l)) init_state final_state)

(* Branch backward when equal *)
let test_exec_beq2=
    let rf_i = rf_update_many [(8, 42l); (9, 42l)] empty_rf in
    let init_state  = { r = rf_i; m = empty_mem; pc = 8l } in
    let final_state = { r = rf_i; m = empty_mem; pc = 0l } in
    (mk_exec_test (Beq(R8, R9, -2l)) init_state final_state)

(* Do not branch as not equal *)
let test_exec_beq3=
    let rf_i = rf_update_many [(8, 42l); (9, (-42l))] empty_rf in
    let init_state  = { r = rf_i; m = empty_mem; pc = 0l } in
    let final_state = { r = rf_i; m = empty_mem; pc = 4l } in
    (mk_exec_test (Beq(R8, R9, 4l)) init_state final_state)

let test_exec_jr =
    let rf_i = (rf_update 10 40l empty_rf) in
    let init_state =  { r = rf_i; m = empty_mem; pc = 0l } in
    let final_state = { r = rf_i; m = empty_mem; pc = 40l } in
    (mk_exec_test (Jr(R10)) init_state final_state)

let test_exec_jal = 
    let rf_i = empty_rf in
    let rf_f = (rf_update 31 4l empty_rf) in
    let init_state =  { r = rf_i; m = empty_mem; pc = 0l } in
    let final_state = { r = rf_f; m = empty_mem; pc = 84l } in
    (mk_exec_test (Jal(84l)) init_state final_state)

let test_exec_li = 
    let rf_i = empty_rf in
    let rf_f = (rf_update 8 256l empty_rf) in
    let init_state =  { r = rf_i; m = empty_mem; pc = 0l } in
    let final_state = { r = rf_f; m = empty_mem; pc = 4l } in
    (mk_exec_test (Li(R8, 256l)) init_state final_state)

(* Load word, 42,  located at offset 0x14 in memory *)
let test_exec_lw =
    let rf_i = (rf_update 8 4l empty_rf) in
    let rf_f = (rf_update 9 42l rf_i) in
    let init_mem = mem_update_many [ (20l, (mk_byte 0x00l)); 
                                     (21l, (mk_byte 0x00l));
                                     (22l, (mk_byte 0x00l));
                                     (23l, (mk_byte 0x2al)) ] empty_mem in
    let init_state =  { r = rf_i; m = init_mem; pc = 0l } in
    let final_state = { r = rf_f; m = init_mem; pc = 4l } in
    (mk_exec_test (Lw(R9, R8, 16l)) init_state final_state)

(* Store $8 = 42 into memory at address 0x14 *)
let test_exec_sw = 
    let rf_i = (rf_update_many [ (8, 42l); (9, 4l)] empty_rf) in
    let final_mem = mem_update_many [ (8l, (mk_byte 0x00l)); 
                                      (9l, (mk_byte 0x00l));
                                      (10l, (mk_byte 0x00l));
                                      (11l, (mk_byte 0x2al)) ] empty_mem in
    let init_state =  { r = rf_i; m = empty_mem; pc = 0l } in
    let final_state = { r = rf_i; m = final_mem; pc = 4l } in
    (mk_exec_test (Sw(R8, R9, 4l)) init_state final_state)

(* Store $10 = 256 into memory at address 0x64 *)
let test_exec_sw2 = 
    let rf_i = (rf_update_many [(10, 256l); (9, 4l)] empty_rf) in
    let final_mem = mem_update_many [ (64l, (mk_byte 0x00l)); 
                                      (65l, (mk_byte 0x00l));
                                      (66l, (mk_byte 0x01l));
                                      (67l, (mk_byte 0x00l)) ] empty_mem in
    let init_state =  { r = rf_i; m = empty_mem; pc = 0l } in
    let final_state = { r = rf_i; m = final_mem; pc = 4l } in
    (mk_exec_test (Sw(R10, R9, 60l)) init_state final_state)
    
let exec_tests = [ test_exec_add;
                   test_exec_ori;
                   test_exec_lui;
                   test_exec_beq1;
                   test_exec_beq2;
                   test_exec_beq3;
                   test_exec_jr;
                   test_exec_jal;
                   test_exec_li;
                   test_exec_lw;
                   test_exec_sw;
                   test_exec_sw2;]
  
(* Functional Tests *)
let test_update_mem = 
    (mk_verbose_expect_test 
	    (fun () ->
	    let init_state = {m = empty_mem; pc = 0l; r = empty_rf} in
	    let test_inst  = Add(R4, R5, R6)                        in
	    let new_state  = (inst_update_mem test_inst init_state) in 
	    (* We're looking for 0x00A62020 split into 4 bytes *)
	        ( word_mem_lookup 0l new_state.m ) )
        0x00A62020l
        Int32.to_string
        "Update Memory"
    )        

let test_assemble_prog = fun () -> 
    let test_program    = [ Add(R4, R5, R6);  Lw(R10, R6, 4l); Sw(R15, R9, 8l) ] in 
    let new_state = (assem test_program) in 
    (* We're looking for 0x00A62020 split into 4 bytes *)
        (  (word_mem_lookup 0l new_state.m) = 0x00A62020l
        && (word_mem_lookup 4l new_state.m) = 0x8cca0004l
        && (word_mem_lookup 8l new_state.m) = 0xad2f0008l
        && (new_state.pc = 0l) )
;;        

(run_test_set assemble_inst_tests    "Binary Translation Tests") ;;
(run_test_set disassemble_inst_tests "Disassembly Tests") ;;
(run_test_set masker_tests           "Bitmask Creation Tests") ;;
(run_test_set int32_to_int16_tests   "int32 to int16 Tests") ;;
(run_test_set int16_to_int32_tests   "int16 to int32 Tests") ;;
(run_test_set exec_tests             "Exec Tests") ;;
(run_test_set [  test_update_mem; 
                 Test("Assemble Program",  test_assemble_prog) ] 
              "Assembler Functional Tests" ) 
