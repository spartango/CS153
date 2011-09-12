open Mips_ast
open Byte
open Binary_ops

exception TODO
exception FatalError
exception UnalignedAccessError

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

let compare_rf (rf_src : regfile) (rf_dest: regfile) : string = 
    let src_keys = 
        IntMap.fold 
            (fun key v rf -> (rf_update key 0l rf)) 
            rf_src 
            empty_rf 
    in
    let union_keys = 
        IntMap.fold 
            (fun key v rf -> (rf_update key 0l rf)) 
            rf_dest
            src_keys
    in
    (IntMap.fold 
            (fun key v s -> 
                let src_val  = (rf_lookup key rf_src)  in
                let dest_val = (rf_lookup key rf_dest) in  
                if src_val = dest_val 
                then s^""
	            else s^(reg2str (ind2reg (Int32.of_int key)))^": "
	                  ^(Int32.to_string src_val)
	                  ^" vs "^(Int32.to_string dest_val))
            union_keys 
            "")
                  
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

let compare_mem (mem_src : memory) (mem_dest: memory) : string = 
    ""

(* State *)
type state      = { r : regfile;   pc : int32; m : memory }
let empty_state = { m = empty_mem; pc = 0l;    r = empty_rf}

let string_of_state (s : state) : string = 
    "Memory:\n"         ^(string_of_mem s.m)
    ^"---\nRegisters:\n"^(string_of_rf s.r)
    ^"---\nPc: "        ^(Int32.to_string s.pc)

(* Copies a 32 bit object into adjacent memory locations *)
let word_mem_update (word : int32) (offset : int32) (m : memory) : memory = 
  (* Split into parts by shifting *)   
  (* Insert parts into slots from offset *)
  let mem_1 = (mem_update offset (Byte.mk_byte (Int32.shift_right_logical word 24)) m)                    in
  let mem_2 = (mem_update (Int32.add offset 1l) (Byte.mk_byte (Int32.shift_right_logical word 16)) mem_1) in
  let mem_3 = (mem_update (Int32.add offset 2l) (Byte.mk_byte (Int32.shift_right_logical word 8))  mem_2) in
              (mem_update (Int32.add offset 3l) (Byte.mk_byte word) mem_3)

(* Reads a word starting from the offset in memory *)
let word_mem_lookup (offset : int32) (m : memory) : int32 = 
    left_shift_or [ ((b2i32 (mem_lookup offset m)), 24); 
                    ((b2i32 (mem_lookup (Int32.add offset 1l) m)), 16);
                    ((b2i32 (mem_lookup (Int32.add offset 2l) m)), 8);
                    ((b2i32 (mem_lookup (Int32.add offset 3l) m)), 0) ]
    
(* Translates an instruction to binary and copies it into memory, resolving pseudoinstructions *)
let rec inst_update_mem (target : inst) (s : state) : state = 
    (* Pick out pseudoinstructions *) 
    match target with 
      | Li(rs, imm) -> 
        (* First put an Lui for the upper half of the immediate *) 
        let new_state   = (inst_update_mem (Lui(R1, (int32_upper imm))) s) in
        (* Then tack on an Ori for the lower half *)
        (inst_update_mem (Ori(rs, R1, (int32_lower imm))) new_state)
      (* Do a binary translate & update *)
      | t_inst      ->   { r = s.r; m = (word_mem_update (inst_to_bin target) s.pc s.m); pc = (Int32.add s.pc 4l)}       

(* Map a program, a list of Mips assembly instructions, down to a starting 
   state. You can start the PC at any address you wish. Just make sure that 
   you put the generated machine code where you started the PC in memory! *)
let rec assem (prog : program) : state = 
    (* A nice helper function to accumulate state and move across memory space as it is updated *)
    let rec assem_r (prog : program) (machine_s : state) : state = 
      (* Grab the next instruction *)
      match prog with 
        (* If we're at the end, move the PC to the beginning and return*)
        | [] -> {m = machine_s.m; pc = 0l; r = machine_s.r}
        (* For real instructions *)
        | t_inst :: rest -> 
          (* Encode the part in binary and push the binary into memory at the next free address(es) *)
          let new_state   = (inst_update_mem t_inst machine_s) in
          (* assemble the rest of the program *) 
          (assem_r rest new_state)
    in 
    let init_state = empty_state in
    (assem_r prog init_state)

(* Disassembles a binary word into a MIPS instruction *) 
let disassem (binary : int32) : inst = 
    match (get_opcode binary) with 
        | 0x00l -> (match (get_opcode2 binary) with
              | 0x08l -> Jr(get_reg1 binary)
              | 0x20l -> Add((get_reg3 binary), (get_reg1 binary), (get_reg2 binary))
              | _ -> raise NotRegister)
        (* Left-shift target, as target must be word aligned *)
        | 0x03l -> Jal(Int32.shift_left (Int32.logand binary (masker 26 6)) 2)
        | 0x04l -> Beq((get_reg1 binary), (get_reg2 binary), int16_to_int32 binary)
        | 0x0dl -> Ori((get_reg2 binary), (get_reg1 binary), int16_to_int32 binary)
        | 0x0fl -> Lui((get_reg2 binary), int32_lower binary)
        | 0x23l -> Lw((get_reg2 binary), (get_reg1 binary), int16_to_int32 binary)
        | 0x2bl -> Sw((get_reg2 binary), (get_reg1 binary), int16_to_int32 binary)
        | _ -> raise NotRegister

(* Checks for word alignment of address *)
let check_word_aligned (target_addr : int32) : int32 =
	if (Int32.rem target_addr 4l) != 0l 
	    then raise UnalignedAccessError
	    else
	        target_addr
	    
(* Increments the PC of a state *) 
let increment_pc (machine_s : state) : state = 
   { pc = (Int32.add 4l machine_s.pc); 
     m  = machine_s.m;
     r  = machine_s.r  }

(* Executes a Beq on a given state, branching if equal *)
let exec_beq (rs : reg) (rt : reg) (label : int32) (machine_s : state) : state = 
    if (rf_lookup (reg2ind rs) machine_s.r) = (rf_lookup (reg2ind rt) machine_s.r)
    then { pc = (Int32.add machine_s.pc (Int32.mul label 4l));
           m  = machine_s.m; 
           r  = machine_s.r  }
    else (increment_pc machine_s)

(* Executes a Jr on a given state, jumping to the address stored in rs *)                  
let exec_jr (rs : reg) (machine_s : state) : state = 
	{ pc = (check_word_aligned (rf_lookup (reg2ind rs) machine_s.r)); 
	  m  = machine_s.m;
	  r  = machine_s.r  }

(* Executes a Jal on a given state, jumping to a target and linking the return address *)          
let exec_jal (target : int32) (machine_s : state) : state =
    { pc = (check_word_aligned target); 
      m  = machine_s.m;
      r  = (rf_update (reg2ind R31) (Int32.add machine_s.pc 4l) (machine_s.r))  }

(* Executes a Lui on a given state, loading an immediate into the upper half of a register *)
let exec_lui (rt : reg) (imm : int32) (machine_s : state) : state = 
    increment_pc { pc = machine_s.pc; 
                   m  = machine_s.m; 
                   r  = (rf_update (reg2ind rt) (Int32.shift_left imm 16) (machine_s.r))  }

(* Executes a Ori on a given state, OR-ing an immediate *)                                
let exec_ori (rt : reg) (rs : reg) (imm : int32) (machine_s : state) : state =
	increment_pc { pc = machine_s.pc; 
	               m  = machine_s.m; 
	               r  = (rf_update (reg2ind rt) 
	                    (Int32.logor imm (rf_lookup (reg2ind rs) machine_s.r)) 
	                    machine_s.r )  }    

(* Executes a Lw on a given state, loading a word *)
let exec_lw (rt : reg) (rs : reg) (offset : int32) (machine_s : state) : state =
    let target_addr = (Int32.add (rf_lookup (reg2ind rs) machine_s.r) offset)  in
	    increment_pc { pc = machine_s.pc;
	                   m  = machine_s.m; 
	                   r  = (rf_update (reg2ind rt) 
	                        (word_mem_lookup 
	                            (check_word_aligned target_addr)
	                            machine_s.m) 
	                        machine_s.r )  }

(* Executes a Sw on a given state, storing a word *)                                                
let exec_sw (rt : reg) (rs : reg) (offset : int32) (machine_s : state) : state =
    let target_addr = (Int32.add (rf_lookup (reg2ind rs) machine_s.r) offset)  in
	    increment_pc { pc = machine_s.pc;
	                   m  = (word_mem_update (check_word_aligned target_addr)
	                                         (rf_lookup (reg2ind rt) machine_s.r)
	                                         machine_s.m); 
	                   r  = machine_s.r  }

(* Executes an Add on a given state, adding the targeted registers*)
let exec_add (rd : reg) (rs : reg) (rt : reg) (machine_s : state) : state =
    increment_pc { pc = machine_s.pc;
                   m  = machine_s.m; 
                   r  = (rf_update (reg2ind rd) 
                                   (Int32.add (rf_lookup (reg2ind rs) machine_s.r) 
                                              (rf_lookup (reg2ind rt) machine_s.r)) 
                                   machine_s.r)  } 
                                
(* Executes a Li on a given state, loading a 32 bit li *)                               
let exec_li (rs : reg) (imm : int32) (machine_s : state) : state =    
    increment_pc { pc = machine_s.pc;  
                   m  = machine_s.m; 
                   r  = (rf_update (reg2ind rs) imm (machine_s.r))}                               

let exec (target : inst) (machine_s : state) : state =
    (* Match against possible ops *)
    match target with 
        (* Perform mem/reg operation *)     (* Move PC as necessary (default to +1) *)
        
        (* Branch by offset if rs == rt *)
        (* Jump to the address specified in rs*)
        (* Jump to instruction at target, save address in RA*)
        (* Load immediate into upper half of register*) 
        (* rs | imm -> rt *) 
        (* Load (word) at address into register rt.*) 
        (* Store word from rt at address *)
        (* rs + rt -> rd*)
        
        | Beq(rs, rt, label)  -> (exec_beq rs rt label machine_s)                  
        | Jr(rs)              -> (exec_jr  rs machine_s)
        | Jal(target)         -> (exec_jal target machine_s)
        | Lui(rt, imm)        -> (exec_lui rt imm machine_s)
        | Ori(rt, rs, imm)    -> (exec_ori rt rs imm machine_s)
        | Lw(rt, rs, offset)  -> (exec_lw  rt rs offset machine_s)
        | Sw(rt, rs, offset)  -> (exec_sw  rt rs offset machine_s)
        | Add(rd, rs, rt)     -> (exec_add rd rs rt machine_s) 
        | Li (rs, imm)        -> (exec_li  rs imm machine_s) (* This shouldn't get called with the dissambler in the pipe, but is good for testing *)

(* Given a starting state, simulate the Mips machine code to get a final state *)
let rec interp (init_state : state) : state = 
    (* Grab instruction binary from addresses, concatenating as we go *)
    let bin_inst  = (word_mem_lookup init_state.pc init_state.m ) in
    match bin_inst with 
        | 0l -> (* Noop -> Done *) init_state
        | _  -> 
            (* Disassemble *) 
            let t_inst    = (disassem bin_inst) in
            (* Exec *)
            let new_state = (exec t_inst init_state) in
            (* Handoff state *)
            (interp new_state)
