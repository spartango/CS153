open Mips
open Ast

(* Code Crawling Utilities *)

exception LabelNotFound

(* Finds a label, returning all the instructions that follow it *)
let rec find_label (target : string) (insts : inst list) : inst list = 
	match insts with
	| []                -> []
	| Label(name)::rest -> if (name = target) then rest else (find_label target rest)
	| _::rest           -> (find_label target rest)
;; 

(* Functions that optimize *)

(* Jump threading: eliminating jump->jump behavior *)
let thread_jumps (insts : inst list) : inst list =

	(* Follows jumps until the end, returning the last one *)
	let rec inspect_jump (last_label : string) : string = 
		(* Find the target in all code *)
		let target_inst = (find_label last_label insts) in
		match target_inst with 
		| J(label_name)::_ -> (inspect_jump label_name)
		| _ 			   -> last_label
	in 
	
	(* For each instruction *)
	let rec thread_jumps_r (upper_segment : inst list)  (lower_segment : inst list) =
		(* Check if its a jump *)
		match lower_segment with 
		| []                  -> upper_segment
		| J(label_name)::rest -> thread_jumps_r (upper_segment @ [J(inspect_jump label_name)]) rest
	    | inst::rest          -> thread_jumps_r (upper_segment @ [inst]) rest
	in
	thread_jumps_r [] insts
;; 

