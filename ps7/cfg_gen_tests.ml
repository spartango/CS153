open Test_framework
open Pretty_print
open Cfg_ast
open Cfg_gen
open Io_types
open Test_ioblocks

(* Set to show verbose test results for debugging *)
let mk_test_verbose = false
let show_insts = true
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

let mk_list_compare_tests (f: 'a -> 'b list) (compare: 'b -> 'b -> int) (eq: 'b -> 'b -> bool) 
        (to_string: 'b -> string) (labeler: 'b -> string) (input: 'a) (e: 'b list) (name: string) =
    let ordered_results = List.sort compare (f input) in
    let ordered_expected = List.sort compare e in
        List.fold_left2 (fun tests res exp ->
                             (mk_generic_equals_test eq (fun () -> res) exp to_string (labeler exp))::tests) 
            [] 
            ordered_results 
            ordered_expected

let mk_block_expect_test (b: block) (e: io_block) (name: string) =
    mk_generic_equals_test io_block_equal (fun () -> build_io_block b) e ib2str name

let mk_func_expect_test (f: func) (e: io_block list) (name: string) =
    mk_list_compare_tests (fun b_list -> block_gen_io (List.map build_io_block b_list)) io_block_compare io_block_equal ib2str (fun e -> e.block_label) f e name
                                                  
let mk_io_inst_tests (b: block) (e: io_inst list) (name: string) = 
    mk_list_compare_tests (fun inst_list -> 
                               let result = build_io_block inst_list in
                          result.insts) io_inst_compare io_inst_equal ioinst2str (fun e -> inst2string e.src_inst) b e name

(*
let mk_func_expect_test2 (f: func) (e: io_block list) (name: string) =
        mk_generic_equals_test 
            
            (fun () -> block_gen_io (List.map build_io_block f)) 
            e
            (fun l -> String.concat "\n" (List.map ib2str l)) 
            name
*)




let block1_test = mk_block_expect_test block1 io_block1 "Block 1"
let block2_test = mk_block_expect_test block2 io_block2 "Block 2"
let block3_test = mk_block_expect_test block3 io_block3 "Block 3"
let block4_test = mk_block_expect_test block4 io_block4 "Block 4";;

let func_test = mk_func_expect_test [block1; block2; block3; block4] [io_block1; io_block2; io_block3; io_block4] "Function io block creation test";;
let io_inst1 = mk_io_inst_tests block1 block1_insts_io "io_insts tests";;
let io_insts_test = List.flatten (List.map (fun e ->
                                                let(bs, expcts) = e in
                                                    mk_io_inst_tests bs expcts "io insts tests") [(block1, block1_insts_io);
                                                                                                  (block2, block2_insts_io);
                                                                                                  (block3, block3_insts_io);
                                                                                                  (block4, block4_insts_io)]);;


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

(* 
run_test_set [ block1_test;
               block2_test;
               block3_test;
               block4_test;
             ] "Block generation tests";;
*)


run_test_set func_test "Function io block generation test";;
run_test_set io_insts_test "Examine io insts";;

(*
print_endline (ib2str (io_block_set_insts (inst_gen_io_base io_block4.block_out io_block4.insts) io_block4))
*)
