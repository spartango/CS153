open Pretty_print
open Test_framework
open Cfg_ast
open I_graph
open Cfg_gen
open Test_ioblocks

let raw_blocks = 
  [ Label("main");
    Move( Var("j"), Int(5));
    Move( Var("k"), Int(2));
    Load( Var("g"), Var("j"), 12);
    Arith( Var("h"), Var("k"), Minus, Int(1));
    Arith( Var("f"), Var("g"), Times, Var("h"));
    Load( Var("e"), Var("j"), 8);
    Load( Var("m"), Var("j"), 16  );
    Load( Var("b"), Var("f"), 0 );
    Arith( Var("c"), Var("e"), Plus, Int(8));
    Move( Var("d"), Var("c"));
    Arith( Var("k"), Var("m"), Plus, Int(4));
    Move( Var("j"), Var("b"));  
    Arith( Var("u"), Var("d"), Plus, Var("k"));
    Arith( Var("u1"), Var("j"), Plus, Int(2));
    Move( Reg(Mips.R2), Var("u1"));
    Return 
  ]
;;

let build_edge_set (v: var) (edge_vars: var list) : IGEdgeSet.t =
    List.fold_left (fun set edge_var  -> IGEdgeSet.add { left = edge_var; right = v} set) IGEdgeSet.empty edge_vars

let build_node (v: var) (interferes: var list) : ignode =
    let node1 = new_ignode v in
        ignode_set_edges (build_edge_set v interferes) node1

let node1 = build_node "n1" ["n2"]
let node2 = build_node "n2" ["n1"; "n3"]
let node3 = build_node "n3" ["n2"]
