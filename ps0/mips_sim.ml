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

(* Utility function to get lower bits of a 32 bit int, shedding the sign *)
let int32_lower (n : int32) : int32 = (n land 0x0000FFFFl)

(* Utility function to get upper bits of a 32 bit int*)
let int32_upper (n : int32) : int32 = (n lsr 16) 

(* Copies a 32 bit object into adjacent memory locations *)
let word_mem_update (word : int32) (offset : int32) (m : memory) : memory = 
	(* Split into parts by shifting *)   
	(* Insert parts into slots from offset *)
	let mem_1 = (Int32Map.mem_update offset (Byte.mk_byte (word lsr 24)) m)     in
	let mem_2 = (Int32Map.mem_update offset (Byte.mk_byte (word lsr 16)) mem_1) in
	let mem_3 = (Int32Map.mem_update offset (Byte.mk_byte (word lsr 8))  mem_2) in
	  (Int32Map.mem_update offset (Byte.mk_byte word) mem_3)
	
(* Translates an instruction to binary and copies it into memory, resolving pseudoinstructions *)
let rec inst_update_mem (target : inst) (s : state) : state = 
	  (* Pick out pseudoinstructions *) 
		match inst with 
			| Li(rs, imm) -> 
				(* First put an Lui for the upper half of the immediate *) 
				let new_state   = (inst_update_mem Lui(rs, (int32_upper imm)) s) in
				(* Then tack on an Ori for the lower half *)
				(inst_update_mem Ori(rs, (int32_lower imm)) new_state)
			(* Do a binary translate & update *)
			| t_inst      ->   { r = s.r; m = (word_mem_update (inst_to_bin inst) s.pc s.m); pc = s.pc + 32l}			 

(* Performs instruction to binary translation *) 
let inst_to_bin (target : inst) : int32 = 
  match target with 
    (* Registers are 5 bits unless otherwise indicated *) 
    (* 0(6) rs rt rd 0(5) 0x20(6)-- rs + rt -> rd*)
    | Add(rd, rs, rt)     -> ( 0 lor ((reg2ind rs) lsl 21) lor ((reg2ind rt) lsl 16) lor ((reg2ind rd) lsl 11) lor 0x20 )
    (* 4(6) rs rt offset(16)     -- Branch by offset if rs == rt *)
    | Beq(rs, rt, label) ->  ( 0 lor (4 lsl 26) lor ((reg2ind rs) lsl 21) lor ((reg2ind rt) lsl 16) lor label )
    (* 0(6) rs 0(15) 8(6)        -- Jump to the address specified in rs*)
    | Jr(rs)              -> ( 0 lor ((reg2ind rs) lsl 21) lor 8 )
    (* 3(6) target(26)           -- Jump to instruction at target, save address in RA*) 
    | Jal(target)         -> ( 0 lor (3 lsl 26) lor target )
		(* 0xf(6) 0(5) rt imm(16)    -- Load immediate into upper half of register*) 
		| Lui(rt, imm)        -> ( 0 lor (0xf lsl 26) lor ((reg2ind rt) lsl 16) lor imm )
		(* 0xd(6) rt rs imm(16)      -- rs | imm -> rt *) 
		| Ori(rt, rs, imm)    -> ( 0 lor (0xd lsl 26) lor ((reg2ind rs) lsl 21) lor ((reg2ind rt) lsl 16) lor imm)
    (* 0x23(6) rs rt offset(16)  -- Load (word) at address into register rt.*) 
    | Lw(rs, rt, offset)  -> ( 0 lor (0x23 lsl 26) lor ((reg2ind rs) lsl 21) lor ((reg2ind rt) lsl 16) lor offset )
    (* 0x2b(6) rs rt offset(16)  -- Store word from rt at address *)
    | Sw(rs, rt, offset)  -> ( 0 lor (0x2b lsl 26) lor ((reg2ind rs) lsl 21) lor ((reg2ind rt) lsl 16) lor offset )

(* Map a program, a list of Mips assembly instructions, down to a starting 
   state. You can start the PC at any address you wish. Just make sure that 
   you put the generated machine code where you started the PC in memory! *)
let rec assem (prog : program) : state = 
	  (* A nice helper function to accumulate state and move across memory space as it is updated *)
		let rec assem_r (prog : program) (machine_s : state) : state = 
		  (* Grab the next instruction *)
			match prog with 
				(* If we're at the end, then we're done*)
				| [] -> machine_s
				(* For real instructions *)
				| t_inst :: rest -> 
		      (* Encode the part in binary and push the binary into memory at the next free address(es) *)
					let new_state   = (inst_update_mem t_inst machine_s.pc machine_s.m) in
		      (* assemble the rest of the program *) 
					(assem_r rest new_state)

(* Given a starting state, simulate the Mips machine code to get a final state *)
let rec interp (init_state : state) : state = raise TODO
