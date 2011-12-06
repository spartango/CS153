open Cfg_ast
open Io_types
open I_graph

(* Helper functions *)

(* Performs a map on an inteference graph *)
let igraph_map (f: ignode -> ignode) (graph: interfere_graph) : interfere_graph =
    IGNodeSet.fold (fun node g -> IGNodeSet.add (f node) g) graph IGNodeSet.empty

(* Gets number of edges of a node *)
let count_edges (node: ignode) : int =
    IGEdgeSet.cardinal node.edges

(* Unmarks interfering as interfering with node. *)
(* Makes no change to node if does not already interfere with interfereing *)
let remove_interfere (node: ignode) (interfering: var) : ignode =
    (* Builds edge to remove *)
    let edge = {interfere_var = interfering; node_var = node.name} in
    let new_edge_set = IGEdgeSet.remove edge node.edges in
        (* Return updated node *)
        ignode_set_edges new_edge_set node

(* Takes a node out of the graph, removing edges to it *)
let remove_node (node: ignode) (graph: interfere_graph) : interfere_graph =
    (* Removes node from graph *)
    let updated_graph = IGNodeSet.remove node graph in 
    (* Removes all remaining interference edges to that node in the graph *)
        igraph_map (fun n -> remove_interfere n node.name) updated_graph
