open Mips_ast
open Byte

exception TODO
exception FatalError

(* Register file definitions. A register file is a map from a register 
   number to a 32-bit quantity. *)
module IntMap = Map.Make(struct type t = int let compare = compare end)
	type regfile = int32 IntMap.t 

	let empty_rf = IntMap.empty
	
	let rf_update (r : int) (v : int32) (rf : regfile) : regfile = 
	  IntMap.add r v rf
		
	let rf_lookup (r : int) (rf : regfile) : int32 = 
	  try IntMap.find r rf with Not_found -> Int32.zero
		
	let string_of_rf (rf : regfile) : string = 
	  IntMap.fold (fun key v s -> 
	    s^(string_of_int key)^" -> "^(Int32.to_string v)^"\n") rf ""

(* Memory definitions. A memory is a map from 32-bit addresses to bytes. *)
module Int32Map = Map.Make(struct type t = int32 let compare = Int32.compare end)
	type memory = byte Int32Map.t
	
	let empty_mem = Int32Map.empty
	
	let mem_update (a : int32) (v : byte) (m : memory) : memory =
	  Int32Map.add a v m
		
	let mem_lookup (a : int32) (m : memory) : byte =
	  try (Int32Map.find a m) with Not_found -> mk_byte Int32.zero
		
	let string_of_mem (m : memory) : string =
	  Int32Map.fold (fun key v s ->
	    s^(Int32.to_string key)^" -> "^(Int32.to_string (b2i32 v))^"\n") m ""

(* State *)
type state = { r : regfile; pc : int32; m : memory }

(* Performs instruction to binary translation *) 
let inst_to_bin (target : inst) : int32 = 
	match target with 
		(* Registers are 5 bits unless otherwise indicated *) 
		(* 0(6) rs rt rd 0(5) 0x20 -- rs + rt -> rd*)
		| Add(rd, rs, rt)     -> 
		(* 4(6) rs rt offset(16)   -- branch by offset if rs == rt *)
		| Beq(rd, rs, offset) -> 0
		(* 0(6) rs 0(15) 8(6)      -- jump to the address specified in rs*)
		| Jr(rs)              -> 0
		(* 3(6) target(26)         -- Jump to instruction at target, save address in RA*) 
		| Jal(target)         -> 0
		(* pseudo instruction?! Move the immediate imm into register rdest*)
		| Li(rdest, imm)      -> 0
		(* 0x23(6) rs rt offset    -- Load (word) at address into register rt.*) 
		| Lw(rt, address)     -> 0
		(* 0x2b(6) rs rt offset    -- Store word from rt at address *)
		| Sw(rt, address)     -> 0

(* Map a program, a list of Mips assembly instructions, down to a starting 
   state. You can start the PC at any address you wish. Just make sure that 
   you put the generated machine code where you started the PC in memory! *)
let rec assem (prog : program) : state = raise TODO
	(* Grab the next instruction *)
	  (* If we're at the end, then we're done)*)
		(* For real instructions *)
			(* Encode the part in binary *) 
		  (* Push the binary into memory at the next free address(es) *)
			(* assemble the rest of the program *) 

(* Given a starting state, simulate the Mips machine code to get a final state *)
let rec interp (init_state : state) : state = raise TODO
