open Cfg_ast
open Io_types
open I_graph
open Stack

exception Implement_Me

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
let is_simplifiable (n: ignode) (num_regs: int) : bool = ((count_edges n) < num_regs) && (not (is_move_related n)) && (not (is_colored n))

(* Unmarks interfering as interfering with node. *)
(* Makes no change to node if does not already interfere with interfereing *)
let remove_interfere (node: ignode) (interfering: var) : ignode =
    (* Builds edge to remove *)
    let edge = {interfere_var = interfering; node_var = node.name} in
    let new_edge_set = IGEdgeSet.remove edge node.edges in
        (* Return updated node *)
        ignode_set_edges new_edge_set node

(* Removes move related edge between node.name and move_related *)
let remove_move (node: ignode) (move_related: var) : ignode =
    let move_edge = { node_var = node.name; interfere_var = move_related } in
    let new_move_set = IGMoveSet.remove move_edge node.edges in
        ignode_set_moves new_move_set node

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
                  if (is_simplifiable node num_regs)
                  then 
                      let new_stack = VarStack.push (Single(node.name)) v_stack in
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



(* Checks if two nodes can be coalesced - Will node coalesce nodes that are both move and interfere related*)
let are_coalescable (graph: interfere_graph) (num_regs: int) (node: ignode) (related_node: ignode)  : bool =
    if IGEdgeSet.mem {node_var = node.name; interfere_var = related_node.name} node.edges
        then false
        else 
            IGEdgeSet.for_all 
                (fun e -> 
                     (* Look up interfering var in graph *)
                     let n = get_node (e.interfere_var) graph in
                         (* Return true if node has fewer than k edges or interferes with related_node *)
                         ((count_edges n < num_regs) || IGEdgeSet.mem {node_var = n.name; interfere_var = related_node.name} n.edges)) 
                node.edges

(* Maps coalesced nodes into graph -  updates references in other nodes. Does not remove coalesced_node from graph *)
let map_coalesced (node: var) (coalesced_node: var) (graph: interfere_graph) : interfere_graph =
    (* Updates an edge so its interfere_var points to node.name *)
    let mk_updated_edge (n_var: var) : igedge = {node_var = n_var; interfere_var = node } in
    let update_if_coalesced_edge (n: ignode) : ignode  =
        let old_edge = {node_var = n.name; interfere_var = coalesced_node} in
            if IGEdgeSet.mem old_edge n.edges
            then 
                let updated_edges = IGEdgeSet.add (mk_updated_edge n.name) (IGEdgeSet.remove old_edge n.edges) in
                    (* Remove old edge and replace with updated edge *)
                    ignode_set_edges updated_edges n 
            else 
                n 
    in
    let update_if_coalesced_move (n: ignode) : ignode =
        let old_edge = {node_var = n.name; interfere_var = coalesced_node} in
            if IGEdgeSet.mem old_edge n.moves
            then 
                let updated_edges = IGEdgeSet.add (mk_updated_edge n.name) (IGEdgeSet.remove old_edge n.moves) in
                    (* Remove old edge and replace with updated edge *)
                    ignode_set_moves updated_edges n 
            else        
                n
    in
        igraph_map (fun n -> update_if_coalesced_move (update_if_coalesced_edge n)) graph

(* Merges edge set and move set *)
let combine_nodes (node: ignode) (combined_node: ignode) : ignode =
    (* Sets node var in an edge to node.name *)
    let update_edge (e: igedge) : igedge = 
        {node_var = node.name; interfere_var = e.interfere_var} in 
   (* Updates edges in merged and adds them to s *)
    let update_merge_sets (s : IGEdgeSet.t) (merged: IGEdgeSet.t) : IGEdgeSet.t =
        IGEdgeSet.fold 
            (fun edge combined -> IGEdgeSet.add (update_edge edge) combined) 
            merged s
    in
    (* For each edge in combined_node, set node_var to new node var and add to node's edge set *)
    let combined_edges = update_merge_sets node.edges combined_node.edges in
    let combined_moves = update_merge_sets node.moves combined_node.moves in
        ignode_set_moves combined_moves (ignode_set_edges combined_edges node)

(* Merges contects of coalesced_node into node, updates in graph, and removes coalesced node from graph *)
let coalesce_nodes (node: ignode) (coalesced_node: ignode) (graph: interfere_graph) : interfere_graph =
    let pruned_graph = IGNodeSet.remove coalesced_node graph in
    (* Combine nodes, removing move related edges *)
    let combined_node = combine_nodes 
        (remove_move node coalesced_node.name)
        (remove_move coalesced_node node.name) in
    let updated_graph = update_igraph combined_node pruned_graph in
    (* Map coalescing into graph *)
        map_coalesced node.name coalesced_node.name updated_graph
    



let coalesce (num_regs: int) (graph: interfere_graph) (v_stack: VarStack.t) : interfere_graph * VarStack.t =
    raise Implement_Me
(*
    let move_related_list = igraph_filter_elements graph (fun n -> not (IGMoveSet.is_empty n.moves)) in
    let rec iterate_worklist (l: igedge list) : interfere_graph * VarStack.t =
        match l with
            (* Empty list means no further coalescing possible *)
            | [] -> (graph, v_stack)
            | hd::tl ->
                  (* Get nodes out of graph *)
                  let node = get_node hd.node_var graph in
                  let potential_coalesced_node = get_node hd.interfere_var graph in
                  if (are_coalescable node potential_coalesced_node)
                  then 
                      (* Coalesce nodes in graph *)
                      let coalesced_graph = coalesce_nodes node potential_coalesced_node graph in
                          raise Implement_Me in
        raise Implement_Me
*)
                          (* WE HAVE TO MARK THE NODE AS COALESCED... WE COULD CARRY AROUND AN ENVIRONMENT COALESCED NODES (NO!)
                             OR WE CAN REWORK THE DEFINITION OF NODE (UGH) *)

   

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
