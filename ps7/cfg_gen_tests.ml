open Test_framework
open Pretty_print
open Cfg_ast
open Cfg_gen
open Io_types

(* Set to show verbose test results for debugging *)
let mk_test_verbose = false
let show_insts = false
let show_src_block = true



let mk_generic_equals_test (eq: 'a -> 'a -> bool) (f: unit -> 'a) (expected: 'a) (to_string: 'a -> string) (name: string) =
    let t_test = fun () -> 
        let result  = f ()              in 
        let pass    = eq result expected in
        let message = 
            if pass 
            then "Got expected result -> "^(format_string (to_string result) Bright Green) 
            else "Result doesn't match expected -> "^(format_string (to_string result) Bright Red)
                ^" vs "^(format_string (to_string expected) Bright Green)
        in (pass, message)
    in
        Verbose_Test(name, t_test)

let mk_rw_test (i: inst) (rs: ReadSet.t) (ws: WriteSet.t) (mvs: (var * var) list) (name: string) =
    if mk_test_verbose
    then 
        mk_generic_equals_test 
            io_inst_equal
            (fun () -> get_rw i) 
            { inst_read = rs; inst_write = ws; inst_in = InSet.empty; inst_out = OutSet.empty; inst_move = mvs; src_inst = i}
            ioinst2str
            name
    else 

    Test(name, (fun () ->
                     io_inst_equal (get_rw i) { inst_read = rs; inst_write = ws; inst_in = InSet.empty; inst_out = OutSet.empty; inst_move = mvs; src_inst = i} 
               ))

let rw_test1 = mk_rw_test (Label("l")) ReadSet.empty WriteSet.empty [] "Label";;
let rw_test2 = mk_rw_test (Move(Var("t2"), Var("t1"))) (ReadSet.singleton "t1") (WriteSet.singleton "t2") [("t2", "t1")] "Move";;
let rw_test3 = mk_rw_test (Arith(Var("t3"), Var("t2"), Plus, Var("t1"))) (ReadSet.add "t2" (ReadSet.singleton "t1")) (WriteSet.singleton "t3") [] "Arith, two vars"
let rw_test4 = mk_rw_test (Arith(Var("t3"), Int(3), Plus, Var("t1"))) (ReadSet.singleton "t1") (WriteSet.singleton "t3") [] "Arith, one var";;
let rw_test5 = mk_rw_test (Arith(Var("t3"), Int(3), Plus, Int(4))) (ReadSet.empty) (WriteSet.singleton "t3") [] "Arith, no vars";;
let rw_test6 = mk_rw_test (Load(Var("t2"), Var("t1"), 0)) (ReadSet.singleton "t1") (WriteSet.singleton "t2") [] "Load";;
let rw_test7 = mk_rw_test (Store(Var("t2"), 0, Var("t1"))) (ReadSet.add "t2" (ReadSet.singleton "t1")) WriteSet.empty [] "Store";;
let rw_test8 = mk_rw_test (Call(Var("t1"))) (ReadSet.singleton "t1") WriteSet.empty [] "Call";;
let rw_test9 = mk_rw_test (Jump("label")) ReadSet.empty WriteSet.empty [] "Jump";;
let rw_test10 = mk_rw_test (If(Var("t1"), Eq, Var("t2"), "label1", "label2")) (ReadSet.add "t2" (ReadSet.singleton "t1")) WriteSet.empty [] "If, two vars";;
let rw_test11 = mk_rw_test Return ReadSet.empty WriteSet.empty [] "Return";;

(* io_block Build Tests *)

let ib2str = (ioblock2str show_insts show_src_block)

let mk_block_expect_test (b: block) (e: io_block) (name: string) =
    mk_generic_equals_test io_block_equal (fun () -> build_io_block b) e ib2str name

let mk_func_expect_test (f: func) (e: io_block list) (name: string) =
        mk_generic_equals_test 
            (fun l1 l2 -> equal_lists l1 l2 io_block_compare io_block_equal)
            (fun () -> block_gen_io (List.map build_io_block f)) 
            e
            (fun l -> String.concat "\n" (List.map ib2str l)) 
            name


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
     inst_in     = set_add_all [] InSet.empty;
     inst_out    = set_add_all [] OutSet.empty;
     inst_move   = [];
     src_inst    = b1a}    

let b1b = Move(Var("t2"), Var("t1"))
let b1b_io = 
    {inst_read   = set_add_all ["t1"] ReadSet.empty ;
     inst_write  = set_add_all ["t2"] WriteSet.empty;
     inst_in     = set_add_all ["t1"] InSet.empty;
     inst_out    = set_add_all ["t2"] OutSet.empty;
     inst_move   = [];
     src_inst    = b1b}

let b1c = Arith(Var("t3"), Var("t2"), Times, Int 1)
let b1c_io =
    {inst_read   = set_add_all ["t2"] ReadSet.empty ;
     inst_write  = set_add_all ["t3"] WriteSet.empty;
     inst_in     = set_add_all ["t2"] InSet.empty;
     inst_out    = set_add_all ["t2"] OutSet.empty;
     inst_move   = [];
     src_inst    = b1c}

let b1d = If(Var "t2", Eq, Var "t3", "L2", "L3")
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
        block_out     = set_add_all ["t1"; "t3"] OutSet.empty;
        block_move    = [("t2", "t1")];
        insts         = block1_insts_io;
        src_block     = block1;
        children      = set_add_all ["L2";"L3"] BlockSet.empty
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
     inst_in     = set_add_all [] InSet.empty;
     inst_out    = set_add_all [] OutSet.empty;
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
     inst_in     = set_add_all [] InSet.empty;
     inst_out    = set_add_all [] OutSet.empty;
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
     inst_in     = set_add_all [] InSet.empty;
     inst_out    = set_add_all [] OutSet.empty;
     inst_move   = [];
     src_inst    = b4a}  

let b4b = Move(Var "t2", Var "t1")
let b4b_io =
    {inst_read   = set_add_all ["t1"] ReadSet.empty ;
     inst_write  = set_add_all ["t2"] WriteSet.empty;
     inst_in     = set_add_all ["t1";"t2"] InSet.empty;
     inst_out    = set_add_all [] OutSet.empty;
     inst_move   = [];
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

let block1_test = mk_block_expect_test block1 io_block1 "Block 1"
let block2_test = mk_block_expect_test block2 io_block2 "Block 2"
let block3_test = mk_block_expect_test block3 io_block3 "Block 3"
let block4_test = mk_block_expect_test block4 io_block4 "Block 4";;

let func_test = mk_func_expect_test [block1; block2; block3; block4] [io_block1; io_block2; io_block3; io_block4] "Function io block creation test";;


run_test_set [ rw_test1;
               rw_test2;
               rw_test3;
               rw_test4;
               rw_test5;
               rw_test6;
               rw_test7;
               rw_test8;
               rw_test9;
               rw_test10;
               rw_test11;
             ] "Read Write tests";;
 
run_test_set [ block1_test;
               block2_test;
               block3_test;
               block4_test;
               func_test
             ] "Block generation tests";;
