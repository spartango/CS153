open Int32

open Binary_ops

exception NotRegister

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

(* Translates an int32 into a register *)
let ind2reg (i: int32) : reg = 
    match i with
        | 0l -> R0 | 1l -> R1 | 2l -> R2 | 3l -> R3
        | 4l -> R4 | 5l -> R5 | 6l -> R6 | 7l -> R7
        | 8l -> R8 | 9l -> R9 | 10l -> R10 | 11l -> R11
        | 12l -> R12 | 13l -> R13 | 14l -> R14 | 15l -> R15
        | 16l -> R16 | 17l -> R17 | 18l -> R18 | 19l -> R19
        | 20l -> R20 | 21l -> R21 | 22l -> R22 | 23l -> R23
        | 24l -> R24 | 25l -> R25 | 26l -> R26 | 27l -> R27
        | 28l -> R28 | 29l -> R29 | 30l -> R30 | 31l -> R31
        | _ -> raise NotRegister
(* Utility function that wraps reg2ind to give int32s *)
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

let inst2str target = 
    match target with
    | Beq(rs, rt, label)  -> "Beq"
    | Jr(rs)              -> "Jr"
    | Jal(target)         -> "Jal"
    | Lui(rt, imm)        -> "Lui"
    | Ori(rt, rs, imm)    -> "Ori"
    | Lw(rt, rs, offset)  -> "Lw"
    | Sw(rt, rs, offset)  -> "Sw"
    | Add(rd, rs, rt)     -> "Add"
    | Li (_,_)            -> "Li"

exception UntranslatableError

let inst_to_string (i: inst) : string = 
    match i with
        | Beq(rs, rt, lab) -> "Beq " ^ (reg2str rs) ^ ", " ^ (reg2str rt) ^
              ", " ^ (Int32.to_string lab)
        | Jr(rs) -> "Jr " ^ (reg2str rs)
        | Jal(target) -> "Jal " ^ (Int32.to_string target)
        | Lui(rt, imm) -> "Lui " ^ (reg2str rt) ^ ", " ^ (Int32.to_string imm)
        | Ori(r1, r2, offset) -> "Ori " ^ (reg2str r1) ^ ", " ^ (reg2str r2) ^ 
              ", " ^ (Int32.to_string offset)
        | Lw(rt, rs, offset) -> "Lw " ^ (reg2str rt) ^ ", " ^ (reg2str rs) ^
              ", " ^ (Int32.to_string offset)
        | Sw(rt, rs, offset) -> "Sw " ^ (reg2str rt) ^ ", " ^ (reg2str rs) ^ 
              ", " ^ (Int32.to_string offset)
        | Add(r1, r2, r3) -> "Add " ^ (reg2str r1) ^ ", " ^ (reg2str r2) ^ 
              ", " ^ (reg2str r3)
        | Li(rd, imm) -> "Li " ^ (reg2str rd) ^ ", " ^ (Int32.to_string imm)
              

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
    | Beq(rs, rt, label)  -> left_shift_or [ (4l, 26);  ((reg_to_ind rs), 21); ((reg_to_ind rt), 16) ; ((int32_signed_lower (Int32.div label 4l)), 0)  ]
    | Jr(rs)              -> left_shift_or [ ((reg_to_ind rs), 21); (8l, 0) ]
    | Jal(target)         -> left_shift_or [ (3l,    26);  ((Int32.shift_right_logical target 2), 0) ]
    | Lui(rt, imm)        -> left_shift_or [ (0xfl,  26);  ((reg_to_ind rt), 16); (imm, 0) ]
    | Ori(rt, rs, imm)    -> left_shift_or [ (0xdl,  26);  ((reg_to_ind rs), 21);  ((reg_to_ind rt), 16); (imm, 0) ]
    | Lw(rt, rs, offset)  -> left_shift_or [ (0x23l, 26);  ((reg_to_ind rs), 21);  ((reg_to_ind rt), 16); (offset, 0) ]
    | Sw(rt, rs, offset)  -> left_shift_or [ (0x2bl, 26);  ((reg_to_ind rs), 21);  ((reg_to_ind rt), 16); (offset, 0) ]
    | Add(rd, rs, rt)     -> left_shift_or [ ((reg_to_ind rs), 21); ((reg_to_ind rt), 16); ((reg_to_ind rd), 11); (0x20l, 0) ]
    | Li (_,_)            -> raise UntranslatableError (* We can't translate a pseudoinstruction straight to binary *)

(* Gets the opcode portion of a MIPS word, which is first six bits *)
let get_opcode (word: int32) : int32 = (Int32.shift_right_logical word 26)


let get_reg1 (word: int32) : reg = 
    ind2reg (Int32.shift_right_logical (Int32.logand word 0x03E00000l) 21)

let get_reg2 (word: int32) : reg = 
    ind2reg (Int32.shift_right_logical (Int32.logand word 0x001F0000l) 16)

let get_reg3 (word: int32) : reg = 
    ind2reg (Int32.shift_right_logical (Int32.logand word 0x0000F800l) 11)

(* Bitmask against last five bits of a word *)
let get_opcode2 (word: int32) : int32 = Int32.logand word 0x0000003Fl
