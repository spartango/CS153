open Mips_ast
open Byte
open Binary_ops

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
    let init_state = {m = empty_mem; pc = 0l; r = empty_rf} in
    (assem_r prog init_state)

(* Disassembles an instruction *) 
let disassem (binary : int32) : inst = raise TODO
    (* Mask off top 6 bits *) 
    (* Match against possible ops *) 
        (* if 0, mask off last 6 bits *) 
            (* match jr vs add *)
        (* Grab arguments specifically by masking / shifting*)
        (* Return instruction *)

let exec (target : inst) (machine_s : state) : state = raise TODO
    (* Match against possible ops *)
    (* Perform mem/reg operation *)
    (* Handle errors *)
    (* Move PC as necessary (default to +1) *)
    (* Return state *)

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