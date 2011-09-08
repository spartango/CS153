open Int32
open Binary_ops

type label = string

type reg = R0 
     | R1  (* AT : DO NOT USE ME:  assembler temp *)
     | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 
     | R10 | R11 | R12 | R13 | R14 | R15 | R16 | R17 | R18 | R19
     | R20 | R21 | R22 | R23 | R24 | R25 
     | R26 | R27 (* DO NOT USE ME: reserved for OS *)
     | R28
     | R29 | R30 |R31  (* RA: used for special purposes... *)

let reg2str r = 
  match r with
    R0 -> "$0" | R1 -> "$1" | R2 -> "$2" | R3 -> "$3"
  | R4 -> "$4" | R5 -> "$5" | R6 -> "$6" | R7 -> "$7"
  | R8 -> "$8" | R9 -> "$9" | R10 -> "$10" | R11 -> "$11"
  | R12 -> "$12" | R13 -> "$13" | R14 -> "$14" | R15 -> "$15"
  | R16 -> "$16" | R17 -> "$17" | R18 -> "$18" | R19 -> "$19"
  | R20 -> "$20" | R21 -> "$21" | R22 -> "$22" | R23 -> "$23"
  | R24 -> "$24" | R25 -> "$25" | R26 -> "$26" | R27 -> "$27"
  | R28 -> "$28" | R29 -> "$29" | R30 -> "$30" | R31 -> "$31"
  
let str2reg s =
  match s with
    "$0" -> R0 | "$1" -> R1 | "$2" -> R2 | "$3" -> R3 | "$4" -> R4
  | "$5" -> R5 | "$6" -> R6 | "$7" -> R7 | "$8" -> R8 | "$9" -> R9
  | "$10" -> R10 | "$11" -> R11 | "$12" -> R12 | "$13" -> R13
  | "$14" -> R14 | "$15" -> R15 | "$16" -> R16 | "$17" -> R17
  | "$18" -> R18 | "$19" -> R19 | "$20" -> R20 | "$21" -> R21
  | "$22" -> R22 | "$23" -> R23 | "$24" -> R24 | "$25" -> R25
  | "$26" -> R26 | "$27" -> R27 | "$28" -> R28 | "$29" -> R29
  | "$30" -> R30 | "$31" -> R31
  | _ -> R0

let reg2ind r =
  match r with
    R0 -> 0 | R1 -> 1 | R2 -> 2 | R3 -> 3
  | R4 -> 4 | R5 -> 5 | R6 -> 6 | R7 -> 7
  | R8 -> 8 | R9 -> 9 | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15
  | R16 -> 16 | R17 -> 17 | R18 -> 18 | R19 -> 19
  | R20 -> 20 | R21 -> 21 | R22 -> 22 | R23 -> 23
  | R24 -> 24 | R25 -> 25 | R26 -> 26 | R27 -> 27
  | R28 -> 28 | R29 -> 29 | R30 -> 30 | R31 -> 31

(* Utility function that wraps reg_to_ind to give int32s *)
let reg_to_ind (rs : reg) : int32 = (Int32.of_int (reg2ind rs))
 
(* A small subset of the Mips assembly language *)
type inst =
  Add of reg * reg * reg  
| Beq of reg * reg * int32 (* should be int16, but ocaml doesn't have these *) 
| Jr  of reg
| Jal of int32
| Li  of reg * int32        (* this is a pseudoinstruction *) 
| Lw  of reg * reg * int32  (* should be int16, but ocaml doesn't have these *)
| Sw  of reg * reg * int32  (* and here ... *)
| Lui of reg * int32        (* and here ... *)
| Ori of reg * reg * int32  (* and here ... *)

type program = inst list

exception UntranslatableError

(* Performs machine-instruction to binary translation *) 
let inst_to_bin (target : inst) : int32 = 
  match target with 
    (* Registers are 5 bits unless otherwise indicated *) 

    (* 4(6)    rs rt offset(16)     -- Branch by offset if rs == rt *)
    (* 0(6)    rs 0(15) 8(6)        -- Jump to the address specified in rs*)
    (* 3(6)    target(26)           -- Jump to instruction at target, save address in RA*)       
    (* 0xf(6)  0(5) rt imm(16)      -- Load immediate into upper half of register*) 
    (* 0xd(6)  rt rs imm(16)        -- rs | imm -> rt *) 
    (* 0x23(6) rs rt offset(16)     -- Load (word) at address into register rt.*) 
    (* 0x2b(6) rs rt offset(16)     -- Store word from rt at address *)
    (* 0(6)    rs rt rd 0(5) 0x20(6)-- rs + rt -> rd*)
    | Beq(rs, rt, label)  -> left_shift_or [ (4l, 26);  ((reg_to_ind rs), 21); ((reg_to_ind rt), 16); (label, 0) ]
    | Jr(rs)              -> left_shift_or [ ((reg_to_ind rs), 21); (8l, 0) ]
    | Jal(target)         -> left_shift_or [ (3l,    26);  (target, 0) ]
    | Lui(rt, imm)        -> left_shift_or [ (0xfl,  26);  ((reg_to_ind rt), 16); (imm, 0) ]
    | Ori(rt, rs, imm)    -> left_shift_or [ (0xdl,  26);  ((reg_to_ind rs), 21);  ((reg_to_ind rt), 16); (imm, 0) ]
    | Lw(rs, rt, offset)  -> left_shift_or [ (0x23l, 26);  ((reg_to_ind rs), 21);  ((reg_to_ind rt), 16); (offset, 0) ]
    | Sw(rs, rt, offset)  -> left_shift_or [ (0x2bl, 26);  ((reg_to_ind rs), 21);  ((reg_to_ind rt), 16); (offset, 0) ]
    | Add(rd, rs, rt)     -> left_shift_or [ ((reg_to_ind rs), 21); ((reg_to_ind rt), 16); ((reg_to_ind rd), 11); (0x20l, 0) ]
    | Li (_,_)            -> raise UntranslatableError (* We can't translate a pseudoinstruction straight to binary *)
