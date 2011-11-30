open Pretty_print
open Test_framework
open I_graph
open Cfg_gen

let build_edge_set (v: var) (edge_vars: var list) : IGEdgeSet.t =
    List.fold_left (fun edge_var set -> IGEdgeSet.add { left = edge_var; right = v}) IGEdgeSet.empty edge_vars

let build_node (v: var) (interferes: var list) : ignode =
    let node1 = new_ignode v in
        ignode_set_edges (build_edge_set v interferes) node1

let node1 = build_node "n1" ["n2"]
let node2 = build_node "n2" ["n1"; "n3"]
let node3 = build_node "n3" ["n2"]
