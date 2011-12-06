open I_graph

(* Helper functions *)

(* Gets number of edges of a node *)
let count_edges (n: ignode) : int =
    IGEdgeSet.cardinal n.edges
