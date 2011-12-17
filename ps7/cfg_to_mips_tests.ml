open Pretty_print
open Test_framework
open Cfg
open Cfg_ast
open Cfg_to_mips
module M = Mips

(* CFG test material *)

let label_cfg = Label("main")
let label_mips = [M.Label("main")]

let move1_cfg = Move(Reg(M.R4), Reg(M.R5)) (* Register to register *)
let move1_mips = M.Add(M.R4, M.R5, M.Immed(Int32.zero))

let move2_cfg = Move(Reg(M.R4), Int(42)) (* Int to register *)
let move2_mips = M.Li(M.R4, Int32.of_int 42) 

let add1_cfg = Arith(Reg(M.R4), Reg(M.R5), Plus, Reg(M.R6)) (* Register/register *)
let add1_mips = M.Add(M.R4, M.R5, M.Reg(M.R6))

let add2_cfg = Arith(Reg(M.R4), Reg(M.R5), Plus, Int(42)) (* Int and register *)
let add2_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Add(M.R4, M.R5, M.Reg(M.R1))]

let sub1_cfg = Arith(Reg(M.R4), Reg(M.R5), Minus, Reg(M.R6)) (* Register/register *)
let sub1_mips = M.Sub(M.R4, M.R5, M.R6)

let sub2_cfg = Arith(Reg(M.R4), Reg(M.R5), Minus, Int(42)) (* Int and register *)
let sub2_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Sub(M.R4, M.R5, M.R1)] 

let mul1_cfg = Arith(Reg(M.R4), Reg(M.R5), Times, Reg(M.R6)) (* Register/register *)
let mul1_mips = M.Mul(M.R4, M.R5, M.R6)

let mul2_cfg = Arith(Reg(M.R4), Reg(M.R5), Times, Int(42)) (* Int and register *)
let mul2_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Mul(M.R4, M.R5, M.R1)] 

let div1_cfg = Arith(Reg(M.R4), Reg(M.R5), Div, Reg(M.R6)) (* Register/register *)
let div1_mips = M.Div(M.R4, M.R5, M.R6)

let div2_cfg = Arith(Reg(M.R4), Reg(M.R5), Div, Int(42)) (* Int and register *)
let div2_mips =[
    M.Li(M.R1, Int32.of_int 42);
    M.Div(M.R4, M.R5, M.R1)] 

let load_cfg = Load(Reg(M.R4), Reg(M.R5), 42)
let load_mips = M.Lw(M.R4, M.R5, Int32.of_int 42)

let store_cfg = Store(Reg(M.R5), 42, Reg(M.R4))
let store_mips = M.Sw(M.R4, M.R5, Int32.of_int 42)

let call1_cfg = Call(Lab("f"))   (* Call of a label *)
let call1_mips = M.Jal("f")

let call2_cfg = Call(Reg(M.R4))
let call2_mips = M.Jalr(M.R4, M.R31)

let jump_cfg = Jump("l")
let jump_mips = M.J("l")

let ifrr1_cfg = If(Reg(M.R4), Eq, Reg(M.R5), "f1", "f2") (* EQ, two registers *)
let ifrr1_mips = [
    M.Beq(M.R4, M.R5, "f1");
    M.J("f2")]

let ifrr2_cfg = If(Reg(M.R4), Neq, Reg(M.R5), "f1", "f2") (* Neq, two registers *)
let ifrr2_mips = [
    M.Bne(M.R4, M.R5, "f1");
    M.J("f2")]

let ifrr3_cfg = If(Reg(M.R4), Lt, Reg(M.R5), "f1", "f2") (* Lt, two registers *)
let ifrr3_mips = [
    M.Blt(M.R4, M.R5, "f1");
    M.J("f2")]

let ifrr4_cfg = If(Reg(M.R4), Lte, Reg(M.R5), "f1", "f2") (* Lte, two registers *)
let ifrr4_mips = [
    M.Ble(M.R4, M.R5, "f1");
    M.J("f2")]

let ifrr5_cfg = If(Reg(M.R4), Gt, Reg(M.R5), "f1", "f2") (* Gt, two registers *)
let ifrr5_mips = [
    M.Bgt(M.R4, M.R5, "f1");
    M.J("f2")]

let ifrr6_cfg = If(Reg(M.R4), Gte, Reg(M.R5), "f1", "f2") (* Gte, two registers *)
let ifrr6_mips = [
    M.Bge(M.R4, M.R5, "f1");
    M.J("f2")]

let ifri1_cfg = If(Reg(M.R4), Eq, Int(42), "f1", "f2") (* EQ, register int *)
let ifri1_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Beq(M.R4, M.R1, "f1");
    M.J("f2")]

let ifri2_cfg = If(Reg(M.R4), Neq, Int(42), "f1", "f2") (* Neq, register int *)
let ifri2_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Bne(M.R4, M.R1, "f1");
    M.J("f2")]

let ifri3_cfg = If(Reg(M.R4), Lt, Int(42), "f1", "f2") (* Lt, register int *)
let ifri3_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Blt(M.R4, M.R1, "f1");
    M.J("f2")]

let ifri4_cfg = If(Reg(M.R4), Lte, Int(42), "f1", "f2") (* Lte, register int *)
let ifri4_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Ble(M.R4, M.R1, "f1");
    M.J("f2")]

let ifri5_cfg = If(Reg(M.R4), Gt, Int(42), "f1", "f2") (* Gt, register int *)
let ifri5_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Bgt(M.R4, M.R1, "f1");
    M.J("f2")]

let ifri6_cfg = If(Reg(M.R4), Gte, Int(42), "f1", "f2") (* Gte, register int *)
let ifri6_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Bge(M.R4, M.R1, "f1");
    M.J("f2")]

let ifir1_cfg = If(Int(42), Eq, Reg(M.R4), "f1", "f2") (* EQ, int register *)
let ifir1_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Beq(M.R1, M.R4, "f1");
    M.J("f2")]

let ifir2_cfg = If(Int(42), Neq, Reg(M.R4), "f1", "f2") (* Neq, int register *)
let ifir2_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Bne(M.R1, M.R4, "f1");
    M.J("f2")]

let ifir3_cfg = If(Int(42), Lt, Reg(M.R4), "f1", "f2") (* Lt, int register *)
let ifir3_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Blt(M.R1, M.R4, "f1");
    M.J("f2")]

let ifir4_cfg = If(Int(42), Lte, Reg(M.R4), "f1", "f2") (* Lte, int register *)
let ifir4_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Ble(M.R1, M.R4, "f1");
    M.J("f2")]

let ifir5_cfg = If(Int(42), Gt, Reg(M.R4), "f1", "f2") (* Gt, int register *)
let ifir5_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Bgt(M.R1, M.R4, "f1");
    M.J("f2")]

let ifir6_cfg = If(Int(42), Gte, Reg(M.R4), "f1", "f2") (* Gte, int register *)
let ifir6_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Beq(M.R1, M.R4, "f1");
    M.J("f2")]

let ifii1_cfg = If(Int(42), Eq, Int(42), "f1", "f2") (* Int Int Eq, true *)
let ifii1_mips = [M.J("f1")]

let ifii2_cfg = If(Int(42), Eq, Int(0), "f1", "f2") (* Int Int Eq, false *)
let ifii2_mips = [M.J("f2")]

let ifii3_cfg  = If(Int(42), Neq, Int(0), "f1", "f2") (* Int Int Neq, true *)
let ifii3_mips = [M.J("f1")]

let ifii4_cfg = If(Int(42), Neq, Int(42), "f1", "f2") (* Int Int Neq, false *)
let ifii4_mips = [M.J("f2")]

let ifii5_cfg  = If(Int(42), Lt, Int(100), "f1", "f2") (* Int Int Lt, true *)
let ifii5_mips = [M.J("f1")]

let ifii6_cfg = If(Int(42), Lt, Int(0), "f1", "f2") (* Int Int Lt, false *)
let ifii6_mips = [M.J("f2")]

let ifii7_cfg  = If(Int(42), Lte, Int(100), "f1", "f2") (* Int Int Lte, true *)
let ifii7_mips = [M.J("f1")]

let ifii8_cfg = If(Int(42), Lte, Int(0), "f1", "f2") (* Int Int Lte, false *)
let ifii8_mips = [M.J("f2")]

let ifii9_cfg  = If(Int(42), Gt, Int(0), "f1", "f2") (* Int Int Gt, true *)
let ifii9_mips = [M.J("f1")]

let ifii10_cfg = If(Int(42), Gt, Int(100), "f1", "f2") (* Int Int Gt, false *)
let ifii10_mips = [M.J("f2")]

let ifii11_cfg  = If(Int(42), Gte, Int(0), "f1", "f2") (* Int Int Gte, true *)
let ifii11_mips = [M.J("f1")]

let ifii12_cfg = If(Int(42), Gte, Int(100), "f1", "f2") (* Int Int Gte, false *)
let ifii12_mips = [M.J("f2")]

let return_cfg = Return
let return_mips = [M.Jr(M.R31)]

let mk_to_mips_inst_test (raw_cfg: inst) (expected: Mips.inst list) (name: string) =
    mk_verbose_expect_test (fun () -> inst_to_mips raw_cfg) expected (fun mips_list -> String.concat "" (List.map Mips.inst2string mips_list)) name

let inst_test1 = mk_to_mips_inst_test label_cfg label_mips "Label to mips";;

run_test_set [inst_test1] "CFG to Mips tests"
