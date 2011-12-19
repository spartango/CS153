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
let move1_mips = [M.Add(M.R4, M.R5, M.Immed(Int32.zero))]

let move2_cfg = Move(Reg(M.R4), Int(42)) (* Int to register *)
let move2_mips = [M.Li(M.R4, Int32.of_int 42)]

let add1_cfg = Arith(Reg(M.R4), Reg(M.R5), Plus, Reg(M.R6)) (* Register/register *)
let add1_mips = [M.Add(M.R4, M.R5, M.Reg(M.R6))]

let add2_cfg = Arith(Reg(M.R4), Reg(M.R5), Plus, Int(42)) (* Int and register *)
let add2_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Add(M.R4, M.R5, M.Reg(M.R1))]

let sub1_cfg = Arith(Reg(M.R4), Reg(M.R5), Minus, Reg(M.R6)) (* Register/register *)
let sub1_mips = [M.Sub(M.R4, M.R5, M.R6)]

let sub2_cfg = Arith(Reg(M.R4), Reg(M.R5), Minus, Int(42)) (* Int and register *)
let sub2_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Sub(M.R4, M.R5, M.R1)] 

let mul1_cfg = Arith(Reg(M.R4), Reg(M.R5), Times, Reg(M.R6)) (* Register/register *)
let mul1_mips = [M.Mul(M.R4, M.R5, M.R6)]

let mul2_cfg = Arith(Reg(M.R4), Reg(M.R5), Times, Int(42)) (* Int and register *)
let mul2_mips = [
    M.Li(M.R1, Int32.of_int 42);
    M.Mul(M.R4, M.R5, M.R1)] 

let div1_cfg = Arith(Reg(M.R4), Reg(M.R5), Div, Reg(M.R6)) (* Register/register *)
let div1_mips = [M.Div(M.R4, M.R5, M.R6)]

let div2_cfg = Arith(Reg(M.R4), Reg(M.R5), Div, Int(42)) (* Int and register *)
let div2_mips =[
    M.Li(M.R1, Int32.of_int 42);
    M.Div(M.R4, M.R5, M.R1)] 

let load_cfg = Load(Reg(M.R4), Reg(M.R5), 42)
let load_mips = [M.Lw(M.R4, M.R5, Int32.of_int 42)]

let store_cfg = Store(Reg(M.R5), 42, Reg(M.R4))
let store_mips = [M.Sw(M.R4, M.R5, Int32.of_int 42)]

let call1_cfg = Call(Lab("f"))   (* Call of a label *)
let call1_mips = [M.Jal("f")]

let call2_cfg = Call(Reg(M.R4))
let call2_mips = [M.Jalr(M.R4, M.R31)]

let jump_cfg = Jump("l")
let jump_mips = [M.J("l")]

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
    M.Bge(M.R1, M.R4, "f1");
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

(* Finally, translate the ouptut of reg_alloc to Mips instructions *)
let cfg_to_mips (f : func ) : Mips.inst list = 
    List.fold_right 
        (fun b accumulated_mips -> (block_to_mips b) @ accumulated_mips)
        f
        []

let mk_to_mips_inst_test (raw_cfg: inst) (expected: Mips.inst list) (name: string) =
    mk_verbose_expect_test (fun () -> inst_to_mips raw_cfg) expected (fun mips_list -> String.concat "" (List.map Mips.inst2string mips_list)) name

let mk_to_mips_block_test (raw_cfg: block) (expected: Mips.inst list) (name: string) =
    mk_verbose_expect_test (fun () -> List.concat (List.map inst_to_mips raw_cfg)) expected (fun mips_list -> String.concat "" (List.map Mips.inst2string mips_list)) name

let mk_to_mips_func_test (raw_cfg: func) (expected: Mips.inst list) (name: string) =
    mk_verbose_expect_test 
        (fun () -> List.concat 
             (List.concat
                  (List.map 
                       (fun l ->
                            List.map inst_to_mips l) raw_cfg)))
        expected (fun mips_list -> String.concat "" (List.map Mips.inst2string mips_list)) name

let inst_test1  = mk_to_mips_inst_test label_cfg label_mips "Label to mips";;
let inst_test2  = mk_to_mips_inst_test move1_cfg move1_mips "Move reg to reg";;
let inst_test3  = mk_to_mips_inst_test move2_cfg move2_mips "Move int to reg";;
let inst_test4  = mk_to_mips_inst_test add1_cfg add1_mips "Add reg and reg";;
let inst_test5  = mk_to_mips_inst_test add2_cfg add2_mips "Add reg and int";;
let inst_test6  = mk_to_mips_inst_test sub1_cfg sub1_mips "Sub reg and reg";;
let inst_test7  = mk_to_mips_inst_test sub2_cfg sub2_mips "Sub reg and int";;
let inst_test8  = mk_to_mips_inst_test mul1_cfg mul1_mips "Mul reg and reg";;
let inst_test9  = mk_to_mips_inst_test mul2_cfg mul2_mips "Mul reg and int";;
let inst_test10 = mk_to_mips_inst_test div1_cfg div1_mips "Div reg and reg";;
let inst_test11 = mk_to_mips_inst_test div2_cfg div2_mips "Div reg and int";;
let inst_test12 = mk_to_mips_inst_test load_cfg load_mips "Store";;
let inst_test13 = mk_to_mips_inst_test store_cfg store_mips "Load";;
let inst_test14 = mk_to_mips_inst_test call1_cfg call1_mips "Call of label";;
let inst_test15 = mk_to_mips_inst_test call2_cfg call2_mips "Call of register";;
let inst_test16 = mk_to_mips_inst_test jump_cfg jump_mips "Jump";;
let inst_test17 = mk_to_mips_inst_test return_cfg return_mips "Return";;

let if_test1  = mk_to_mips_inst_test ifrr1_cfg ifrr1_mips "If reg reg eq";;
let if_test2  = mk_to_mips_inst_test ifrr2_cfg ifrr2_mips "If reg reg neq";;
let if_test3  = mk_to_mips_inst_test ifrr3_cfg ifrr3_mips "If reg reg lt";;
let if_test4  = mk_to_mips_inst_test ifrr4_cfg ifrr4_mips "If reg reg lte";;
let if_test5  = mk_to_mips_inst_test ifrr5_cfg ifrr5_mips "If reg reg gt";;
let if_test6  = mk_to_mips_inst_test ifrr6_cfg ifrr6_mips "If reg reg gte";;
let if_test7  = mk_to_mips_inst_test ifri1_cfg ifri1_mips "If reg int eq";;
let if_test8  = mk_to_mips_inst_test ifri2_cfg ifri2_mips "If reg int neq";;
let if_test9  = mk_to_mips_inst_test ifri3_cfg ifri3_mips "If reg int lt";;
let if_test10 = mk_to_mips_inst_test ifri4_cfg ifri4_mips "If reg int lte";;
let if_test11 = mk_to_mips_inst_test ifri5_cfg ifri5_mips "If reg int gt";;
let if_test12 = mk_to_mips_inst_test ifri6_cfg ifri6_mips "If reg int gte";;
let if_test13 = mk_to_mips_inst_test ifir1_cfg ifir1_mips "If int reg eq";;
let if_test14 = mk_to_mips_inst_test ifir2_cfg ifir2_mips "If int reg neq";;
let if_test15 = mk_to_mips_inst_test ifir3_cfg ifir3_mips "If int reg lt";;
let if_test16 = mk_to_mips_inst_test ifir4_cfg ifir4_mips "If int reg lte";;
let if_test17 = mk_to_mips_inst_test ifir5_cfg ifir5_mips "If int reg gt";;
let if_test18 = mk_to_mips_inst_test ifir6_cfg ifir6_mips "If int reg gte";;
let if_test19 = mk_to_mips_inst_test ifii1_cfg ifii1_mips "If int int eq true";;
let if_test20 = mk_to_mips_inst_test ifii2_cfg ifii2_mips "If int int eq false";;
let if_test21 = mk_to_mips_inst_test ifii3_cfg ifii3_mips "If int int neq true";;
let if_test22 = mk_to_mips_inst_test ifii4_cfg ifii4_mips "If int int neq false";;
let if_test23 = mk_to_mips_inst_test ifii5_cfg ifii5_mips "If int int lt true";;
let if_test24 = mk_to_mips_inst_test ifii6_cfg ifii6_mips "If int int lt false";;
let if_test25 = mk_to_mips_inst_test ifii7_cfg ifii7_mips "If int int lte true";;
let if_test26 = mk_to_mips_inst_test ifii8_cfg ifii8_mips "If int int lte false";;
let if_test27 = mk_to_mips_inst_test ifii9_cfg ifii9_mips "If int int gt true";;
let if_test28 = mk_to_mips_inst_test ifii10_cfg ifii10_mips "If int int gt false";;
let if_test29 = mk_to_mips_inst_test ifii11_cfg ifii11_mips "If int int gte true";;
let if_test30 = mk_to_mips_inst_test ifii12_cfg ifii12_mips "If int reg gte false";;

run_test_set [inst_test1;
              inst_test2;
              inst_test3;
              inst_test4;
              inst_test5;
              inst_test6;
              inst_test7;
              inst_test8;
              inst_test9;
              inst_test10;
              inst_test11;
              inst_test12;
              inst_test13;
              inst_test14;
              inst_test15;
              inst_test16;
              inst_test17;
             ] "CFG to Mips tests";;

run_test_set [if_test1;
              if_test2;
              if_test3;
              if_test4;
              if_test5;
              if_test6;
              if_test7;
              if_test8;
              if_test9;
              if_test10;
              if_test11;
              if_test12;
              if_test13;
              if_test14;
              if_test15;
              if_test16;
              if_test17;
              if_test18;
              if_test19;
              if_test20;
              if_test21;
              if_test22;
              if_test23;
              if_test24;
              if_test25;
              if_test26;
              if_test27;
              if_test28;
              if_test29;
              if_test30;
             ] "CFG IF to Mips tests"

let block1_cfg = [label_cfg; add1_cfg; sub1_cfg; ifrr1_cfg]
let block1_mips = label_mips @ add1_mips @ sub1_mips @ ifrr1_mips

let block2_cfg = [label_cfg; add2_cfg; div2_cfg; return_cfg]
let block2_mips = label_mips @ add2_mips @ div2_mips @ return_mips

let block_test1 = mk_to_mips_block_test block1_cfg block1_mips "Block1 test";;
let block_test2 = mk_to_mips_block_test block2_cfg block2_mips "Block2 test";;

run_test_set [block_test1; block_test2] "Block level tests";;

let prog1_cfg = [block1_cfg; block2_cfg]
let prog1_mips = block1_mips @ block2_mips

let prog_test1 = mk_to_mips_func_test prog1_cfg prog1_mips "Program 1 test";;

run_test_set [prog_test1] "Program level tests"

