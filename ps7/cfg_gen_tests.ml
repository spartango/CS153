open Test_framework
open Pretty_print
open Cfg_ast
open Cfg_gen
open Io_types

(* Set to show verbose test results for debugging *)
let mk_test_verbose = false

let mk_rw_test (i: inst) (rs: ReadSet.t) (ws: WriteSet.t) (mvs: (var * var) list) (name: string) =
    if mk_test_verbose
    then 
        let expected = { inst_read = rs; inst_write = ws; inst_in = InSet.empty; inst_out = OutSet.empty; inst_move = mvs; src_inst = i} in
        let to_string = ioinst2str in
        let t_test = fun () -> 
            let result  = get_rw i            in 
            let pass    = io_inst_equal result expected in
            let message = 
                if pass 
                then "Got expected result -> "^(format_string (to_string result) Bright Green) 
                else "Result doesn't match expected -> "^(format_string (to_string result) Bright Red)
                     ^" vs "^(format_string (to_string expected) Bright Green)
            in (pass, message)
        in
            Verbose_Test(name, t_test)
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

(* 
 * Label L1
 * t2 = t1
 * t3 = t2 * 1
 * if t2 = t3 then L2 else L3
 *)

let block1 =
    [Label("L1");
     Move(Var("t2"), Var("t1"));
     Arith(Var("t3"), Var("t2"), Times, Int 1);
     If(Var "t2", Eq, Var "t3", "L2", "L3")]

(*
 * L4
 * t2 = t1;
 * J L3
 *)
let block4 =
    [Label("L4");
     Move(Var "t2", Var "t1");
     Jump("L3")]

(*
 * L2
 * t5 = t2 +7;
 * t4 = t5 + t5
 * t6 = 4 + 5
 * Return
 *)

let block2 =
    [Label("L2");
     Arith(Var "t5", Var "t2", Plus, Int 7);
     Arith(Var "t4", Var "t5", Plus, Var "t5");
     Arith(Var "t6", Int 4, Plus, Int 5);
     Return]

(* L3
 * t7 =t3 + t2
 * return
 *)

let block3 =
    [Label("L3");
     Arith(Var "t7", Var "t3", Plus, Var "t2");
     Return];;


let io_block1 =
    {
        block_label   = "L1";
        master_reads  = varset_add ["t1";"t2";"t3"] ReadSet.empty;
        master_writes = varset_add ["t2"; "t3"] WriteSet.empty;
        block_in      = varset_add ["t1"] InSet.empty;
        block_out     = varset_add ["t1"; "t3"] OutSet.empty;
        block_move    = [("t2", "t1")];
        insts         = [];
        src_blcok     = block1;
        children      = varset_add ["L2";"L4"] BlockSet.empty
     };;



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
    
