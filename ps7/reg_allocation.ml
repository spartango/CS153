open Cfg_ast
open Io_types
open I_graph
open Stack

(* Helper functions *)
 
(* Performs a map on an inteference graph *)
let igraph_map (f: ignode -> ignode) (graph: interfere_graph) : interfere_graph =
    IGNodeSet.fold (fun node g -> IGNodeSet.add (f node) g) graph IGNodeSet.empty

(* Returns a list of the nodes that satisfy p *)
let igraph_filter_elements (g: interfere_graph) (p: ignode -> bool) : ignode list =
    IGNodeSet.elements (IGNodeSet.filter p g)


(* Gets number of edges of a node *)
let count_edges (node: ignode) : int =
    IGEdgeSet.cardinal node.edges

let is_move_related (node: ignode) : bool =
    not (IGMoveSet.is_empty node.moves)

let is_colored (node: ignode) : bool =
    match node.color with 
        | None -> false
        | Some i -> true

(* Determines whether it is is safe to remove a given element from the graph *)
let is_removable (n: ignode) (num_regs: int) : bool = ((count_edges n) < num_regs) && (not (is_move_related n)) && (not (is_colored n))

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

(* Reduces graph until all non-move-related/non-pre-colored nodes have more than number_registers edges *)
let simplify (num_regs: int) (initial_graph: interfere_graph) : interfere_graph * VarStack.t =

    (* Performs one round of simplification over the graph *)
    let rec simplify_r (g: interfere_graph) (v_stack: VarStack.t) (work_stack: ignode list) : interfere_graph * VarStack.t =
        match work_stack with 
            | [] -> (g, v_stack)
            | node::work_stack_tail ->
                  if (is_removable node num_regs)
                  then 
                      let new_stack = VarStack.push node.name v_stack in
                      let new_graph = remove_node node g in
                          simplify_r new_graph new_stack work_stack_tail
                  else 
                      simplify_r g v_stack work_stack_tail in

    (* Performs simplification on the grpah until it is stable *)
    let rec loop (g: interfere_graph) (v_stack: VarStack.t) : interfere_graph * VarStack.t =
        let work_stack = IGNodeSet.elements g in
        let (new_graph, new_v_stack) = simplify_r g v_stack work_stack in
            if new_graph = g
            then 
                (* Graph is stable, so simplification is over *)
                (new_graph, new_v_stack)
            else
                (* Do another round of simplification *)
                loop new_graph new_v_stack in
            loop initial_graph VarStack.empty
                 
    (* Iterates over graph until finds removable node *)

(* ALGORITHM FOR REGISTER ALLOCATION *)
(* MASTER GRAPH - original interference graph that should not be modified *)
(* WORK GRAPH - graph passed between functions as we try to reduce it *)

(* Simplify graph - remove non-move-related nodes with fewer than k edges *)
            (* Get list of element in graph *)
            (* Run through list of elements *)
                 (* If an element has fewer than k edges and is not move related or pre-colored *)
                     (* Add node to stack *)
                     (* Remove node from graph *)
                     (* Continue to element using new graph  *)
                 (* Else continue to next node *)
            (* Check if graph has changed at all *)

(* Coalesce move related nodes *)
            (* Make worklist of move-related elements *)
            (* Run through list of node, checking if each can be coalesced *)
                (* For each move-related edge *)
                     (* If each neighbor of the node (x) has fewer than k edges or interferes with other y *)
                         (* Coalesce y into x - remove y and replace with x *)
                         (* Remove move-related edge *)
                         (* Simplify result and re-coalesce - i.e. throw away worklist *)
                     (* Else continue to next edge *)
            (* Once no more edges to coalesce, return working graph in current state *)
