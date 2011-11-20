open Test_framework
open Pretty_print
open Cfg_ast
open Cfg_gen
open Io_types

(* Set to show verbose test results for debugging *)
let mk_test_verbose = true

let mk_rw_test (i: inst) (rs: ReadSet.t) (ws: WriteSet.t) (mvs: (var * var) list) (name: string) =
    if mk_test_verbose
    then 
        let t_test = fun () -> 
            let result  =               in 
            let pass    = (result = expected) in
            let message = 
                if pass 
                then "Got expected result -> "^(format_string (to_string result) Bright Green) 
                else "Result doesn't match expected -> "^(format_string (to_string result) Bright Red)
                     ^" vs "^(format_string (to_string expected) Bright Green)
        in (pass, message)
    in
    Verbose_Test(name, t_test)


        mk_verbose_expect_test (fun () -> get_rw i) { inst_read = rs; inst_write = ws; inst_in = InSet.empty; inst_out = OutSet.empty; inst_move = mvs; src_inst = i} ioinst2str name
    else 

    Test(name, (fun () ->
                     io_inst_equal (get_rw i) { inst_read = rs; inst_write = ws; inst_in = InSet.empty; inst_out = OutSet.empty; inst_move = mvs; src_inst = i} 
               ))

let rw_test1 = mk_rw_test (Label("l")) ReadSet.empty WriteSet.empty [] "Label";;
let rw_test2 = mk_rw_test (Move(Var("t2"), Var("t1"))) (ReadSet.singleton "t1") (WriteSet.singleton "t2") [("t1", "t2")] "Move";;
let rw_test3 = mk_rw_test (Arith(Var("t3"), Var("t2"), Plus, Var("t1"))) (ReadSet.add "t2" (ReadSet.singleton "t1")) (WriteSet.singleton "t3") [] "Arith, two vars"
let rw_test4 = mk_rw_test (Arith(Var("t3"), Int(3), Plus, Var("t1"))) (ReadSet.singleton "t1") (WriteSet.singleton "t3") [] "Arith, one var";;
let rw_test5 = mk_rw_test (Arith(Var("t3"), Int(3), Plus, Int(4))) (ReadSet.empty) (WriteSet.singleton "t3") [] "Arith, no vars";;
let rw_test6 = mk_rw_test (Load(Var("t2"), Var("t1"), 0)) (ReadSet.singleton "t1") (WriteSet.singleton "t2") [] "Load";;
let rw_test7 = mk_rw_test (Store(Var("t2"), 0, Var("t1"))) (ReadSet.add "t2" (ReadSet.singleton "t1")) WriteSet.empty [] "Store";;
let rw_test8 = mk_rw_test (Call(Var("t1"))) (ReadSet.singleton "t1") WriteSet.empty [] "Call";;
let rw_test9 = mk_rw_test (Jump("label")) ReadSet.empty WriteSet.empty [] "Jump";;
let rw_test10 = mk_rw_test (If(Var("t1"), Eq, Var("t2"), "label1", "label2")) (ReadSet.add "t2" (ReadSet.singleton "t1")) WriteSet.empty [] "If, two vars";;
let rw_test11 = mk_rw_test Return ReadSet.empty WriteSet.empty [] "Return";;

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
    
