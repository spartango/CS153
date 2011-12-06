open I_graph

(* Helper functions *)

(* Performs a map on an inteference graph *)
let igraph_map (f: ignode -> ignode) (graph: interfere_graph) : interfere_graph =
    IGNodeSet.fold (fun node g -> IGNodeSet.add (f node) g) graph IGNodeSet.empty

(* Gets number of edges of a node *)
let count_edges (n: ignode) : int =
    IGEdgeSet.cardinal n.edges

(* Remove an edge from a node. For edge {left = l;right = r}, will also remove {left = r; right = l} *)
let remove_edge (n: ignode) (e: igedge) : ignode =
    (* Creates the inverse of the edge *)
    let rev_edge = { left = e.right; right = e.left } in
    let new_edge_set = IGEdgeSet.remove e (IGEdgeSet.remove rev_edge n.edges) in
        (* Return node *)
        ignode_set_edges new_edge_set n
