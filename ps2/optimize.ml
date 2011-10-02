open Mips
open Ast
open Compile

(* Functions that optimize *)

(* Jump threading: eliminating jump->jump behavior *)
let thread_jumps (insts : inst list) : inst list =
	(* For each instruction *)
	(* Check if its a jump *)
	(* Find the target *)
	(* Inspect it: *)
		(* If the target starts with a jump *)
		(* Change this jump to the second target *)
		(* Inspect *)

