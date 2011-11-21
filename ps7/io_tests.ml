open Cfg_ast
open Io_types
open Cfg_gen
open Test_framework
open Pretty_print

(* Makes a test that expects a particular value from f, and prints differences
 * it fails to match that value *)
let mk_verbose_varset_test (f : unit -> VarSet.t) (expected : VarSet.t) (name : string) : test = 
    let t_test = fun () -> 
        let result  = (f ())              in 
        let pass    = (VarSet.equal result expected) in
        let message = 
            if pass 
                then "Got expected result -> "^(format_string (varset2str result) Bright Green) 
                else "Result doesn't match expected -> "^(format_string (varset2str result) Bright Red)
                     ^" vs "^(format_string (varset2str expected) Bright Green)
        in (pass, message)
    in
    Verbose_Test(name, t_test)

(* Tests for IN and OUT set generation *)

  (* Simple In Test
   * R : x, y, z 
   * W : a l x 
   * No Outset
   * I should be : x y z *)
  
let simple_in_test = 
  mk_verbose_varset_test 
    (fun () -> 
      let out_set = OutSet.empty                                in
      let read    = set_add_all ["x"; "y"; "z";] ReadSet.empty  in
      let write   = set_add_all ["x"; "a"; "l";] WriteSet.empty in 
      (gen_in out_set read write) )
    (set_add_all ["x"; "y"; "z";] InSet.empty)
    ("Simple IN Set Generation")
;;

(* Simple In Test with Out 
   * R : x, y, z 
   * W : a l x 
   * Predefined O: "f"
   * I should be : x y z f *)

let simple_in_out_test = 
  mk_verbose_varset_test 
    (fun () -> 
      let out_set = set_add_all ["f";]           OutSet.empty   in
      let read    = set_add_all ["x"; "y"; "z";] ReadSet.empty  in
      let write   = set_add_all ["x"; "a"; "l";] WriteSet.empty in 
      (gen_in out_set read write) 
    )
    (set_add_all ["x"; "y"; "z"; "f";] InSet.empty)
    ("Simple IN-Out Set Generation")
;;

(* Simple Out Test 
   * Two Children
   * I_1 = a x y
   * I_2 = z a 
   * O should be a x z y *)
    
let simple_out_test = 
  mk_verbose_varset_test
  (fun () -> 
    let child_1_in = set_add_all ["x"; "y"; "a";] InSet.empty in
    let child_2_in = set_add_all ["z"; "a";]      InSet.empty in
    let children   = [child_1_in; child_2_in;]                in
    (gen_out children)
  )
  (set_add_all ["a"; "z"; "x"; "y";] OutSet.empty)
  "Simple Out Set Generation"
;;

(* Simple Out-In Test 
   * Two Children
   * I_1 = a x y
   * I_2 = z a 
   * *
   * R : x, y 
   * W : a l x 
   * O should be a x z y
   * I should be x y z after iterating *)
    
let simple_out_in_test = 
  mk_verbose_varset_test
  (fun () -> 
    let child_1_in = set_add_all ["x"; "y"; "a";] InSet.empty    in
    let child_2_in = set_add_all ["z"; "a";]      InSet.empty    in
    let read       = set_add_all ["x"; "y";]      ReadSet.empty  in
    let write      = set_add_all ["x"; "a"; "l";] WriteSet.empty in 
    let children   = [child_1_in; child_2_in;]                   in
    let out_set    = (gen_out children) in
    (gen_in out_set read write)
  )
  (set_add_all ["z"; "x"; "y";] InSet.empty)
  "Simple Out-In Set Generation"
;;

(* Label and children getter tests *)

(* 
 * Label L1
 * t2 = t1
 * t3 = t2 * 1
 * if t2 = t3 then L2 else L3
 *)

let basic_block1 =
    [Label("L1");
     Move(Var("t2"), Var("t1"));
     Arith(Var("t3"), Var("t2"), Times, Int 1);
     If(Var "t2", Eq, Var "t3", "L2", "L3")]

(*
 * Label L5
 * Return
 *)

let basic_block2 =
    [Label("L5");
     Return;]

(* Label L4
 * t2 = t1;
 * Jump L3 
 *)

let basic_block3 = 
    [Label("L4");
     Move(Var("t2"), Var("t1"));
     Jump("L3")]

let mk_label_test (b: block) (e: label) (name: string) =
    mk_verbose_expect_test (fun () -> get_block_label b) e (fun s -> s) name

let mk_children_test (b: block) (e: BlockSet.t) (name: string) =
    mk_verbose_varset_test (fun () -> get_block_children b) e name

let label_test1 = mk_label_test basic_block1 "L1" "Label extraction test";;
let label_test2 = mk_label_test basic_block2 "L5" "Label extraction test";;
let label_test3 = mk_label_test basic_block3 "L4" "Label extraction test";;

let children_test1 = mk_children_test basic_block1 (BlockSet.add "L2" (BlockSet.singleton "L3")) "Child extraction test on If";;
let children_test2 = mk_children_test basic_block2 BlockSet.empty "Child extraction test on Return";;
let children_test3 = mk_children_test basic_block3 (BlockSet.singleton "L3") "Child extraction test on Jump";;


run_test_set [ simple_in_test;
               simple_in_out_test;
               simple_out_test;
               simple_out_in_test
             ]
             "IO Set Generation"
           ;;

run_test_set [
    label_test1;
    label_test2;
    label_test3;
    children_test1;
    children_test2;
    children_test3;
] "Children and Label Getters"
