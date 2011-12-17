open Pretty_print
open Test_framework
open Cfg
open Cfg_ast
open Cfg_to_mips
module M = Mips

(* CFG test material *)

let label_cfg = Lab("main")
let label_mips = M.Label("main")

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

let call2_cfg = Call(Reg(M.$4))
let call2_mips = M.Jalr(M.R4)

let jump_cfg = Jump("l")
let jump_mips = M.J("l")

let if1_cfg = If(Reg(M.R4), Eq, Reg(M.R5), "f1", "f2")
let if1_mips = [
    M.Beq(M.R4, M.R5, "f1");
    M.J("f2")]
