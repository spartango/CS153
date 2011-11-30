open Pretty_print
open Test_framework
open Cfg_ast
open I_graph
open Cfg_gen
open Test_ioblocks

let build_edge_set (v: var) (edge_vars: var list) : IGEdgeSet.t =
    List.fold_left (fun set edge_var  -> IGEdgeSet.add { left = edge_var; right = v} set) IGEdgeSet.empty edge_vars

let build_node (v: var) (interferes: var list) : ignode =
    let node1 = new_ignode v in
        ignode_set_edges (build_edge_set v interferes) node1

let node1 = build_node "n1" ["n2"]
let node2 = build_node "n2" ["n1"; "n3"]
let node3 = build_node "n3" ["n2"]
