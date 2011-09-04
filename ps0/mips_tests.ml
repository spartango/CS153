open Mips_sim
open Mips_ast

let test_inst_to_bin : bool = 
	let test_inst = Add(R4, R5, R6) in
	let binary    = inst_to_bin test_inst in
	  binary = 0xA62020
		
(* TODO Call tests *)

let _ = print_string "Tests complete"