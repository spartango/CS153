open I_graph

(* Helper functions *)

(* Performs a map on an inteference graph *)
let igraph_map (f: ignode -> ignode) (graph: interfere_graph) : interfere_graph =
    IGNodeSet.fold (fun node g -> IGNodeSet.add (f node) g) graph IGNodeSet.empty

(* Gets number of edges of a node *)
let count_edges (node: ignode) : int =
    IGEdgeSet.cardinal node.edges

(* Remove an edge from a node. For edge {left = l;right = r}, will also remove {left = r; right = l} *)
let remove_edge (node: ignode) (edge: igedge) : ignode =
    (* Creates the inverse of the edge *)
    let rev_edge = { left = edge.right; right = edge.left } in
    let new_edge_set = IGEdgeSet.remove edge (IGEdgeSet.remove rev_edge node.edges) in
        (* Return node *)
        ignode_set_edges new_edge_set node

(* Takes a node out of the graph, removing edges to it *)
let remove_node (node: ignode) (graph: interfere_graph) : interfere_graph =
    let updated_graph = IGNodeSet.remove node graph in 
    (* Folds over a set of edges
