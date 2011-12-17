open Mips
open Utility
open Cfg_ast
open Word32

exception Invalid_operand

(* Implements translation to mips at block level *)
(*
  Label of label
| Move of operand * operand                     (* x := y *)
| Arith of operand * operand * arithop * operand (* x := y + z *)
| Load of operand * operand * int               (* x := *(y+i) *)
| Store of operand * int * operand              (* *(x+i) := y *)
| Call of operand                               (* invoke f--result in R2 *)
| Jump of label  (* j L *)
| If of operand * compareop * operand * label * label
      (* if x < y then goto L1 else goto L2 *)
| Return  (* return to caller -- result assumed in R2 *)

type compareop = Eq | Neq | Lt | Lte | Gt | Gte
type arithop = Plus | Minus | Times | Div 
    *)

(* Gets a register out of an operand when the operand can only be a register *)
let to_mips_reg (o: operand) : Mips.reg =
    match o with
        | Reg(r) -> r
        | _ -> raise Invalid_operand

(* Transforms a cfg operand to a Mips operand *)
let to_mips_operand (o: operand) : Mips.operand =
    match o with
        | Reg(r) -> Mips.Reg(r)
        | Int(i) -> Mips.Immed(Int32.of_int i)
        | _ -> raise Invalid_operand

let to_mips_label (o: operand) : Mips.label =
    match o with
        | Lab(l) -> l
        | _ -> raise Invalid_operand

let inst_to_mips (i: inst) : Mips.inst list =
    match i with
        | Label l ->
              [Mips.Label(l)]
        | Move(dest,src) ->
              (* Use addition of zero as substitute for move *)
              [Mips.Add(
                   to_mips_reg(dest),
                   to_mips_reg(src),
                   Mips.Immed(Int32.zero)
                  )]
        | Arith(dest, src1, operation, src2) ->
              let mk_arith (d: Mips.reg) (r1: Mips.reg) (r2: Mips.reg) : Mips.inst =
                  (match operation with
                      | Plus  -> Mips.Add(d, r1, Mips.Reg(r2))
                      | Minus -> Mips.Sub(d, r1, r2)
                      | Times -> Mips.Mul(d, r1, r2)
                      | Div   -> Mips.Div(d, r1, r2)) in
                  (* Split if src2 is a regsiter or an immediate *)
                  (match src2 with
                      | Reg(r) -> 
                            [(mk_arith 
                                  (to_mips_reg dest) 
                                  (to_mips_reg src1) 
                                  r)]
                      | Int(i) ->
                            let load_inst = Mips.Li(Mips.R1, Int32.of_int i) in
                                load_inst::[(mk_arith 
                                                (to_mips_reg dest)
                                                (to_mips_reg src1)
                                                Mips.R1)]
                      | _ -> raise Invalid_operand)
        | Load(dest_reg, src_mem, offset) ->
              [Mips.Lw(
                  to_mips_reg(dest_reg),
                  to_mips_reg(src_mem),
                  Int32.of_int offset)]
        | Store(dest_mem, offset, src_reg) ->
              [Mips.Sw(
                  to_mips_reg(src_reg),
                  to_mips_reg(dest_mem),
                  Int32.of_int offset)]
        | Call(f) ->
              (* Jump to label f. Put return address in $ra/$31.
               * Calling convention handled in conversion to cfg. 
               * Assumes f returns result to $2 - again, not dealt with*)
              [Mips.Jal(to_mips_label f)]
        | Jump(l) ->
              [Mips.J(l)]
        (* Check this implementation. I believe this is legal because we are
           replicating what the assembler would be doing if Beq, etc could
           accept an immediate or a register for the second argument *)
        | If(src1, comparison, src2, label1, label2) ->
              (* if (comparison src1 src2), jump label1 else jump label2 *)
              let false_branch = Mips.J(label2) in
                  (* Creates the test instruction, branching if true *)
              let test (r1: Mips.reg) (comp: compareop) (r2: Mips.reg) (l: label) = 
                  match comp with
                      | Eq ->  Mips.Beq(r1, r2, l)
                      | Neq -> Mips.Bne(r1, r2, l)
                      | Lt ->  Mips.Blt(r1, r2, l)
                      | Lte -> Mips.Ble(r1, r2, l)
                      | Gt ->  Mips.Bgt(r1, r2, l)
                      | Gte -> Mips.Bge(r1, r2, l) in
              let build_test (r1: Mips.reg) (c: compareop) (op2: operand) (l: label) =
                  (match op2 with 
                      | Reg(r2) ->
                            [(test r1 c r2 l)]
                      | Int(i) ->
                            (* Load i into $t1 *)
                            let load_inst = Mips.Li(Mips.R1, Int32.of_int i) in
                                load_inst::[(test r1 c Mips.R1 l)]
                      | _ -> raise Invalid_operand)
              in
                  (build_test (to_mips_reg src1) comparison src2 label1) @ [false_branch]
        | Return ->
              (* Jump to register in return slot $R31 *)
              [Mips.Jr(Mips.R31)]

let block_to_mips (b: block) : Mips.inst list =
    (* Take advantage of the fact that fold left reverses list *)
    List.fold_left 
        (fun accumulated_mips i -> (inst_to_mips i) @ accumulated_mips)
        []
        b
                        
