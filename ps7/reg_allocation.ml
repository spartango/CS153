open Cfg_ast
open Io_types
open I_graph
open Stack

(* Container for passing around a graph and the var stack associated with it *)
type reduction_state = {reduce_igraph : interfere_graph; var_stack : VarStack.t}

(* Makes a new reduction state with *)
let mk_reduction_state (g: interfere_graph) (v_stack: VarStack.t) : reduction_state = 
    {
        reduce_igraph = g;
        var_stack = v_stack;
    }

let empty_reduction_state = mk_reduction_state IGNodeSet.empty VarStack.empty
let reduction_set_igraph (graph: interfere_graph) (rs: reduction_state) : reduction_state =
    {
        reduce_igraph = graph;
        var_stack = rs.var_stack
    }
let reduction_set_var_stack (v_stack : VarStack.t) (rs: reduction_state) : reduction_state =
    {
        reduce_igraph = rs.igraph;
        var_stack = v_stack
    }
let initial_reduction_state (graph: interfere_graph) : reduction_state =
    reduction_set_igraph graph empty_reduction_state

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

(* Removes the move related edge between node1 and node2, if it exists *)
let remove_move_edge (var1: var) (var2: var) (graph: interfere_graph) : interfere_graph =
    let node1 = get_node var1 graph in
    let node2 = get_node var2 graph in
    (* Remove potential move related edge from each node *)
    let updated_node1 = remove_move node1 var2 in
    let updated_node2 = remove_move node2 var1 in
    (* Return updated graph *)
        update_igraph updated_node1 (update_igraph updated_node2 graph)

(* Push a node onto a var stack, check to see if that node is coalesced *)
let push_node (node: ignode) (v_stack: VarStack.t) : VarStack.t =
    match node.coalesced with
        | None -> VarStack.push (Single(node.name)) v_stack
        | Some coalesced_vars -> VarStack.push (Coalesced(node.name::coalesced_vars)) v_stack

(* Reduces graph until all non-move-related/non-pre-colored nodes have more than number_registers edges *)
let simplify (num_regs: int) (initial_state: reduction_state) : reduction_state =

    (* Performs one round of simplification over the graph *)
    let rec simplify_r (reduce_state: reduction_state) (worklist: ignode list) : reduction_state =
        match worklist with 
            | [] -> reduce_state
            | n::worklist_tail ->
                  if (is_simplifiable n num_regs)
                  then 
                      let new_stack = push_node n reduce_state.var_stack in
                      let new_graph = remove_node n reduce_state.reduce_igraph in
                          simplify_r (mk_reduction_state new_graph new_stack) worklist_tail
                  else 
                      simplify_r g v_stack worklist_tail in

    (* Performs simplification on the grpah until it is stable *)
    let rec loop (reduce_state: reduction_state) : reduction_state =
        let worklist = IGNodeSet.elements reduce_state.reduce_graph in
        let new_state = simplify_r reduce_state worklist in
            if new_state.reduce_igraph = reduce_state.reduce_igraph
            then 
                (* Graph is stable, so simplification is over *)
                reduce_state
            else
                (* Do another round of simplification *)
                loop new_state in

            loop initial_state



(* Checks if two nodes can be coalesced - Will node coalesce nodes that are both move and interfere related*)
let are_coalescable (graph: interfere_graph) (num_regs: int) (node: ignode) (related_node: ignode) : bool =
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

(* Merges contects of the node for coalesced_var into node of var and places in graph, removes move related edge, and removes coalesced node from graph *)
let coalesce_nodes (var: var) (coalesced_var: var) (master_graph: interfere_graph) : interfere_graph =
    (* Remove move related edge in graph *)
    let graph = remove_move_edge var coalesced_var master_graph in
    (* Look up nodes in graph *)
    let node = get_node var graph in
    let coalesced_node = get_node coalesced_var graph in
    (* Remove coalesced_node from graph *)
    let graph = IGNodeSet.remove coalesced_node graph in
    (* Combine nodes, removing move related edges *)
    let combined_node = combine_nodes node coalesced_node in
    let graph = update_igraph combined_node graph in
    (* Map coalescing into graph *)
        map_coalesced var coalesced_var graph
    



let rec coalesce (num_regs: int) (initial_state: reduction_state : reduction_state =
    let move_related_list = igraph_filter_elements initial_state.reduce_igraph (fun n -> not (IGMoveSet.is_empty n.moves)) in
    let move_edge_list = List.fold_left
        (fun node accum_list -> (IGNodeSet.elements node)::accum_list) 
        [] 
        move_related_list in
                                             
    let rec iterate_worklist (edgelist: igedge list) : reduction_state =
        match edgelist with
            (* Empty list means no nodes in work list could be coaleasced *)
            | [] -> reduction_state
            | edge::edgelist_tail ->
                  (* Get nodes out of graph *)
                  let node = get_node edge.node_var initial_state.reduce_graph in
                  let potential_coalesced_node = get_node edge.interfere_var initial_state.reduce_graph in
                  if (are_coalescable node potential_coalesced_node)
                  then 
                      (* Coalesce nodes in graph *)
                      let coalesced_graph = coalesce_nodes node potential_coalesced_node graph in
                      (* Re-simplify graph and start coalescing from beginning *)
                          coalesce num_regs (simplify num_regs (reduction_set_igraph coalesced_graph initial_state))
                  else 
                      (* Nodes are not coalesable, so move to next node in list *)
                      iterate_worklist edgelist_tail 
    in
        iterate_worklist move_edge_list
   

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
