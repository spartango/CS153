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

  (* Simple Test *)
  (* R : x, y, z 
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

run_test_set [ simple_in_test;
             ]
             "IO Set Generation"
           ;;