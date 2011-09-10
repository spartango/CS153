open Mips_sim
open Mips_ast
open Test_framework

(* add $4 $5 $6 -> 0x00a62020 *)
(* ori $6 $5 34 -> 0x34a60022 *)

(* Assembly Tests for each instruction *)
let mk_inst_to_bin_test (test_inst : inst) (expected : int32) =
    (mk_verbose_expect_test (fun () -> (inst_to_bin test_inst)) 
                           expected 
                           Int32.to_string
                           ("Translate "^(inst2str test_inst)) )

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
    (mk_inst_to_bin_test (Beq(R18, R6, -28l)) 0x1246fff9l)

let test_beq_translate = 
    (mk_inst_to_bin_test (Beq(R18, R6, 4l)) 0x12460001l)

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

let test_add_disassem =
  (mk_verbose_expect_test (fun () -> disassem 0x00a62020l) (Add(R4, R5, R6))
       inst_to_string "Disassemble Add")

let test_ori_disassem =
  (mk_verbose_expect_test (fun () -> disassem 0x34a60022l) (Ori(R6, R5, 34l))
       inst_to_string "Disassemble Ori")

let disassemble_inst_tests = [ test_add_disassem; 
                            test_ori_disassem  ]

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
(run_test_set [  test_update_mem; 
                 Test("Assemble Program",  test_assemble_prog) ] 
              "Assembler Functional Tests" ) 
