open Int32

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
