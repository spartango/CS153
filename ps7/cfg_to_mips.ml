open Mips
open Utility
open Cfg_ast
open Word32

exception Invalid_operand

(* Implements translation to mips at block level *)

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
              (* Use addition if src is a register, Li if src is an int *)
              (match src with
                  | Reg(r) -> 
                        [Mips.Add(
                             to_mips_reg(dest),
                             r,
                             Mips.Immed(Int32.zero)
                         )]
                  | Int(n) ->
                        [Mips.Li(to_mips_reg(dest), Int32.of_int n)]
                  | _ -> raise Invalid_operand)

              (* Use addition of zero as substitute for move *)
              
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
              (* Jump to f. f can be a label or a regsiter
               *  Put return address in $ra/$31.
               * Calling convention handled in conversion to cfg. 
               * Assumes f returns result to $2 - again, not dealt with*)
              (match f with
                  | Lab(l) -> [Mips.Jal(l)]
                  | Reg(r) -> [Mips.Jalr(r, Mips.R31)]
                  | _ -> raise Invalid_operand)
              
        | Jump(l) ->
              [Mips.J(l)]
        (* Check this implementation. I believe this is legal because we are
           replicating what the assembler would be doing if Beq, etc could
           accept an immediate or a register for the second argument *)
        | If(src1, comparison, src2, label1, label2) ->
              (* if (comparison src1 src2), jump label1 else jump label2 *)
              let true_branch = Mips.J(label1) in
              let false_branch = Mips.J(label2) in
                  (* Creates the test instruction, branching if true *)
              let load_inst (i: int) : Mips.inst = 
                  Mips.Li(Mips.R1, Int32.of_int i) in
              let test (r1: Mips.reg) (r2: Mips.reg) = 
                  (match comparison with
                      | Eq ->  Mips.Beq(r1, r2, label1)
                      | Neq -> Mips.Bne(r1, r2, label1)
                      | Lt ->  Mips.Blt(r1, r2, label1)
                      | Lte -> Mips.Ble(r1, r2, label1)
                      | Gt ->  Mips.Bgt(r1, r2, label1)
                      | Gte -> Mips.Bge(r1, r2, label1)) in
                 (match (src1, src2) with
                      | (Reg(r1), Reg(r2)) ->
                             [test r1 r2; false_branch]
                      | (Int(i), Reg(r2)) ->
                            [load_inst i; test Mips.R1 r2; false_branch]
                      | (Reg(r1), Int(i2)) ->
                             [load_inst i2; test r1 Mips.R1; false_branch]
                      | (Int(i1), Int(i2)) ->
                            let optimize (comp_op: int -> int -> bool) : Mips.inst list =
                                if comp_op i1 i2
                                then [true_branch]
                                else [false_branch] in
                            (* This is a hacked optimization *)
                            (match comparison with 
                                 | Eq -> optimize  (=)                           
                                 | Neq -> optimize (<>)
                                 | Lt -> optimize  (<)
                                 | Lte -> optimize (<=)
                                 | Gt -> optimize  (>)
                                 | Gte -> optimize (>=))
                      | (_,_) -> raise Invalid_operand)
        | Return ->
              (* Jump to register in return slot $R31 *)
              [Mips.Jr(Mips.R31)]

let block_to_mips (b: block) : Mips.inst list =
    (* Take advantage of the fact that fold left reverses list *)
    List.fold_left 
        (fun accumulated_mips i -> (inst_to_mips i) @ accumulated_mips)
        []
        b
                        
