open Cfg_ast
open Cfg_gen
open Io_types


(* 
 * Label L1
 * t2 = t1
 * t3 = t2 * 1
 * if t2 = t3 then L2 else L3
 *)

let b1a = Label("L1")
let b1a_io = 
    {inst_read   = set_add_all [] ReadSet.empty ;
     inst_write  = set_add_all [] WriteSet.empty;
     inst_in     = set_add_all ["t1"] InSet.empty;
     inst_out    = set_add_all ["t1"] OutSet.empty;
     inst_move   = [];
     src_inst    = b1a}    

let b1b = Move(Var("t2"), Var("t1"))
let b1b_io = 
    {inst_read   = set_add_all ["t1"] ReadSet.empty ;
     inst_write  = set_add_all ["t2"] WriteSet.empty;
     inst_in     = set_add_all ["t1"] InSet.empty;
     inst_out    = set_add_all ["t2"] OutSet.empty;
     inst_move   = [("t2", "t1")];
     src_inst    = b1b}

let b1c = Arith(Var("t3"), Var("t2"), Times, Int 1)
let b1c_io =
    {inst_read   = set_add_all ["t2"] ReadSet.empty ;
     inst_write  = set_add_all ["t3"] WriteSet.empty;
     inst_in     = set_add_all ["t2"] InSet.empty;
     inst_out    = set_add_all ["t2";"t3"] OutSet.empty;
     inst_move   = [];
     src_inst    = b1c}

let b1d = If(Var "t2", Eq, Var "t3", "L2", "L4")
let b1d_io =
    {inst_read   = set_add_all ["t2"; "t3"] ReadSet.empty ;
     inst_write  = set_add_all [] WriteSet.empty;
     inst_in     = set_add_all ["t2";"t3"] InSet.empty;
     inst_out    = set_add_all [] OutSet.empty;
     inst_move   = [];
     src_inst    = b1d}  

let block1 = [b1a; b1b; b1c; b1d]
let block1_insts_io = [b1a_io; b1b_io; b1c_io; b1d_io]

let io_block1 = 
    {
        block_label   = "L1";
        master_read   = set_add_all ["t1";"t2";"t3"] ReadSet.empty;
        master_write  = set_add_all ["t2"; "t3"] WriteSet.empty;
        block_in      = set_add_all ["t1"] InSet.empty;
        block_out     = set_add_all ["t1"; "t3"; "t2"] OutSet.empty;
        block_move    = [("t2", "t1")];
        insts         = block1_insts_io;
        src_block     = block1;
        children      = set_add_all ["L2";"L4"] BlockSet.empty
     };;




(*
 * L2
 * t5 = t2 +7;
 * t4 = t5 + t5
 * t6 = 4 + 5
 * Return
 *)


let b2a = Label("L2")
let b2a_io = 
    {inst_read   = set_add_all [] ReadSet.empty ;
     inst_write  = set_add_all [] WriteSet.empty;
     inst_in     = set_add_all ["t2"] InSet.empty;
     inst_out    = set_add_all ["t2"] OutSet.empty;
     inst_move   = [];
     src_inst    = b2a}       

let b2b = Arith(Var "t5", Var "t2", Plus, Int 7)
let b2b_io =
    {inst_read   = set_add_all ["t2"] ReadSet.empty ;
     inst_write  = set_add_all ["t5"] WriteSet.empty;
     inst_in     = set_add_all ["t2"] InSet.empty;
     inst_out    = set_add_all ["t5"] OutSet.empty;
     inst_move   = [];
     src_inst    = b2b}

let b2c = Arith(Var "t4", Var "t5", Plus, Var "t5")
let b2c_io =
    {inst_read   = set_add_all ["t5"] ReadSet.empty ;
     inst_write  = set_add_all ["t4"] WriteSet.empty;
     inst_in     = set_add_all ["t5"] InSet.empty;
     inst_out    = set_add_all [] OutSet.empty;
     inst_move   = [];
     src_inst    = b2c}

let b2d = Arith(Var "t6", Int 4, Plus, Int 5)
let b2d_io =
    {inst_read   = set_add_all [] ReadSet.empty ;
     inst_write  = set_add_all ["t6"] WriteSet.empty;
     inst_in     = set_add_all [] InSet.empty;
     inst_out    = set_add_all [] OutSet.empty;
     inst_move   = [];
     src_inst    = b2d}    

let b2e = Return
let b2e_io =
    {inst_read   = set_add_all [] ReadSet.empty ;
     inst_write  = set_add_all [] WriteSet.empty;
     inst_in     = set_add_all [] InSet.empty;
     inst_out    = set_add_all [] OutSet.empty;
     inst_move   = [];
     src_inst    = b2e}      

let block2 = [b2a; b2b; b2c; b2d; b2e]
let block2_insts_io = [b2a_io; b2b_io; b2c_io; b2d_io; b2e_io]

let io_block2 = 
    {
        block_label   = "L2";
        master_read   = set_add_all ["t2";"t5"] ReadSet.empty;
        master_write  = set_add_all ["t5"; "t4";"t6"] WriteSet.empty;
        block_in      = set_add_all ["t2"] InSet.empty;
        block_out     = set_add_all [] OutSet.empty;
        block_move    = [];
        insts         = block2_insts_io;
        src_block     = block2;
        children      = set_add_all [] BlockSet.empty
     };;


(* L3
 * t7 =t3 + t2
 * return
 *)

let b3a = Label("L3")
let b3a_io =
    {inst_read   = set_add_all [] ReadSet.empty ;
     inst_write  = set_add_all [] WriteSet.empty;
     inst_in     = set_add_all ["t3";"t2"] InSet.empty;
     inst_out    = set_add_all ["t3";"t2"] OutSet.empty;
     inst_move   = [];
     src_inst    = b3a}  

let b3b = Arith(Var "t7", Var "t3", Plus, Var "t2")
let b3b_io =
    {inst_read   = set_add_all ["t3";"t2"] ReadSet.empty ;
     inst_write  = set_add_all ["t7"] WriteSet.empty;
     inst_in     = set_add_all ["t3";"t2"] InSet.empty;
     inst_out    = set_add_all [] OutSet.empty;
     inst_move   = [];
     src_inst    = b3b}   
  
let b3c = Return
let b3c_io =
    {inst_read   = set_add_all [] ReadSet.empty ;
     inst_write  = set_add_all [] WriteSet.empty;
     inst_in     = set_add_all [] InSet.empty;
     inst_out    = set_add_all [] OutSet.empty;
     inst_move   = [];
     src_inst    = b3c} 

let block3 = [b3a; b3b; b3c]
let block3_insts_io = [b3a_io; b3b_io; b3c_io]

let io_block3 = 
    {
        block_label   = "L3";
        master_read   = set_add_all ["t3";"t2"] ReadSet.empty;
        master_write  = set_add_all ["t7"] WriteSet.empty;
        block_in      = set_add_all ["t3";"t2"] InSet.empty;
        block_out     = set_add_all [] OutSet.empty;
        block_move    = [];
        insts         = block3_insts_io;
        src_block     = block3;
        children      = set_add_all [] BlockSet.empty
     };;

(*
 * L4
 * t2 = t1;
 * J L3
 *)

let b4a = Label("L4")
let b4a_io =
    {inst_read   = set_add_all [] ReadSet.empty ;
     inst_write  = set_add_all [] WriteSet.empty;
     inst_in     = set_add_all ["t1"] InSet.empty;
     inst_out    = set_add_all ["t1"] OutSet.empty;
     inst_move   = [];
     src_inst    = b4a}  

let b4b = Move(Var "t2", Var "t1")
let b4b_io =
    {inst_read   = set_add_all ["t1"] ReadSet.empty ;
     inst_write  = set_add_all ["t2"] WriteSet.empty;
     inst_in     = set_add_all ["t1"] InSet.empty;
     inst_out    = set_add_all [] OutSet.empty;
     inst_move   = [("t2", "t1")];
     src_inst    = b4b}   
  
let b4c = Jump("L3")
let b4c_io =
    {inst_read   = set_add_all [] ReadSet.empty ;
     inst_write  = set_add_all [] WriteSet.empty;
     inst_in     = set_add_all [] InSet.empty;
     inst_out    = set_add_all [] OutSet.empty;
     inst_move   = [];
     src_inst    = b4c} 

let block4 = [b4a; b4b; b4c]
let block4_insts_io = [b4a_io; b4b_io; b4c_io]

let io_block4 = 
    {
        block_label   = "L4";
        master_read   = set_add_all ["t1"] ReadSet.empty;
        master_write  = set_add_all ["t2"] WriteSet.empty;
        block_in      = set_add_all ["t1";"t3"] InSet.empty;
        block_out     = set_add_all ["t3";"t2"] OutSet.empty;
        block_move    = [("t2","t1")];
        insts         = block4_insts_io;
        src_block     = block4;
        children      = set_add_all ["L3"] BlockSet.empty
     };;
