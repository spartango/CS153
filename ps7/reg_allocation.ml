open Cfg_ast
open Io_types
open I_graph
open Stack

let number_registers = 28

(* Helper functions *)

(* Gets a single element off the igraph. returns an tuple of a node and the remaining igraph or None if empty igraph *)

let get_node (graph: interfere_graph) : (ignode * interfere_graph) option =
    try
        let node = IGNodeSet.choose graph in
            Some (node, (IGNodeSet.remove node graph))
    with
            Not_found -> None
 
(* Performs a map on an inteference graph *)
let igraph_map (f: ignode -> ignode) (graph: interfere_graph) : interfere_graph =
    IGNodeSet.fold (fun node g -> IGNodeSet.add (f node) g) graph IGNodeSet.empty

(* Gets number of edges of a node *)
let count_edges (node: ignode) : int =
    IGEdgeSet.cardinal node.edges

let is_move_related (node: ignode) : bool =
    not (IGMoveSet.is_empty node.moves)

let is_colored (node: ignode) : bool =
    match node.color with 
        | None -> false
        | Some i -> true

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

(* Simplify graph *)
            (* Loop over interference graph *)
            (* If an element has fewer than k edges and is not move related or pre-colored *)
                 (* Add node to stack *)
                 (* Remove node from graph *)
                 (* Start folding again on new graph *)
            (* Else continue to next node *)

(* Reduces graph until all non-move-related/non-pre-colored nodes have more than number_registers edges *)
let rec simplify (num_regs: int) (graph: interfere_graph) (v_stack: VarStack.t) : interfere_graph * VarStack.t =
    let is_removable (n: ignode) : bool = (count_edges n) < num_regs || not (is_move_related n) || not (is_colored n) in
        match (get_node graph) with
            (* None means that the graph is empty and there are no more nodes to possibly reduce *)
            | None -> (graph, v_stack)
            | Some(node, remainder) ->
                  if (is_removable node)
                  then 
                      let new_stack = VarStack.push node.name v_stack in
                      let new_graph = remove_node node remainder in
                          simplify num_regs new_graph new_stack
                  else 
                      simplify num_regs remainder v_stack
    (* Iterates over graph until finds removable node *)

(* ALGORITHM FOR REGISTER ALLOCATION *)


