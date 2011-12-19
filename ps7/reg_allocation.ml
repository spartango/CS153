open Cfg_ast
open Io_types
open I_graph
open Stack

(* Debug tools *)
let print_graph (g: interfere_graph) (l: string) : unit =
    print_endline (l ^ ":\n" ^ (igraph2str g))

let print_var (v: var) (l: string) : unit =
    print_endline (l ^ ": " ^ v ^ "\n")

let print_node (n: ignode) (l: string) : unit =
    print_endline (l ^ ":\n" ^ (ignode2str n))

(* Container for passing around a graph and the var stack associated with it *)
type reduction_state = {reduce_igraph : interfere_graph; var_stack : VarStack.t; register_count : int}

(* Makes a new reduction state with *)
let mk_reduction_state (g: interfere_graph) (v_stack: VarStack.t) (regs: int) : reduction_state = 
    {
        reduce_igraph = g;
        var_stack = v_stack;
        register_count = regs
    }

let empty_reduction_state = mk_reduction_state IGNodeSet.empty VarStack.empty 0
let reduction_set_igraph (graph: interfere_graph) (rs: reduction_state) : reduction_state =
    {
        reduce_igraph = graph;
        var_stack = rs.var_stack;
        register_count = rs.register_count
    }
let reduction_set_var_stack (v_stack : VarStack.t) (rs: reduction_state) : reduction_state =
    {
        reduce_igraph = rs.reduce_igraph;
        var_stack = v_stack;
        register_count = rs.register_count;
    }
let reduction_set_register_count (regs : int) (rs: reduction_state) : reduction_state =
    {
        reduce_igraph = rs.reduce_igraph;
        var_stack = rs.var_stack;
        register_count = regs;
    }
let initial_reduction_state (graph: interfere_graph) (regs: int) : reduction_state =
    reduction_set_register_count regs (reduction_set_igraph graph empty_reduction_state)

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
let is_simplifiable (n: ignode) (num_regs: int) : bool = 
    ((count_edges n) < num_regs) && (not (is_move_related n)) && (not (is_colored n))

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
    let new_move_set = IGMoveSet.remove move_edge node.moves in
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

(* Reduces graph until all non-move-related/non-pre-colored nodes have more than number_registers edges *)
let simplify (initial_state: reduction_state) : reduction_state =

    (* Performs one round of simplification over the graph *)
    let rec simplify_r (reduce_state: reduction_state) (worklist: ignode list) : reduction_state =
        match worklist with 
            | [] -> reduce_state
            | n::worklist_tail ->
                  if (is_simplifiable n initial_state.register_count)
                  then 
                      let new_stack = push_node n reduce_state.var_stack in
                      let new_graph = remove_node n reduce_state.reduce_igraph in
                          simplify_r (mk_reduction_state new_graph new_stack initial_state.register_count) worklist_tail
                  else 
                      simplify_r reduce_state worklist_tail in

    (* Performs simplification on the grpah until it is stable *)
    let rec loop (reduce_state: reduction_state) : reduction_state =
        let worklist = IGNodeSet.elements reduce_state.reduce_igraph in
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
let are_coalescable (graph: interfere_graph) (num_regs: int) (node_v: var) (related_var: var) : bool =
    (* Get node out of graph *)
    let node = get_node node_v graph in
    if IGEdgeSet.mem {node_var = node_v; interfere_var = related_var} node.edges
        then false
        else 
            IGEdgeSet.for_all 
                (fun e -> 
                     (* Look up interfering var in graph *)
                     let n = get_node (e.interfere_var) graph in
                         (* Return true if node has fewer than k edges or interferes with related_node *)
                         ((count_edges n < num_regs) || IGEdgeSet.mem {node_var = n.name; interfere_var = related_var} n.edges)) 
                node.edges

(* Replaces old_edge in set with new_edge if old exists *)
let update_edge_set (s: IGEdgeSet.t) (old_edge: igedge) (new_edge: igedge): IGEdgeSet.t =
    (* Check if old_edge is part of set *)
    if IGEdgeSet.mem old_edge s
    then 
        (* Remove old_edge from set and add new_edge *)
        let pruned_s = IGEdgeSet.remove old_edge s in
            IGEdgeSet.add new_edge pruned_s 
    else 
        (* Returns set unchanged *)
        s


(* Replaces old_var with new_var where it occurs in the nodes of graph *)
let replace_var (old_var: var) (new_var: var) (graph: interfere_graph) : interfere_graph =
    
    let update_node (node: ignode) : ignode =
        (* Check if node had a move to eliminated and replace *)
        (* What the old edge would have looked like, if it existed *)
        let old_edge = {node_var = node.name; interfere_var = old_var} in
        let new_edge = {node_var = node.name; interfere_var = new_var} in
        (* Update the edge and move sets *)
        let updated_edges = update_edge_set node.edges old_edge new_edge in
        let updated_moves = update_edge_set node.moves old_edge new_edge in
            ignode_set_moves updated_moves (ignode_set_edges updated_edges node) 
    in
        igraph_map update_node graph

(* Merges combined_node into node *)
let combine_nodes (node: ignode) (combined_node: ignode) : ignode =
    (* Changes the node_var to node.name *)
    let new_edge (old_edge : igedge) : igedge =
        {node_var = node.name; interfere_var = old_edge.interfere_var} in 
    let update_merge_sets (base_set : IGEdgeSet.t) (set_to_add: IGEdgeSet.t) : IGEdgeSet.t =
        IGEdgeSet.fold 
            (fun edge combined -> IGEdgeSet.add (new_edge edge) combined) 
            set_to_add base_set
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
    (* Replace occurances of coalesced_node with node in the graph *)
        replace_var coalesced_var var graph
    
(* Get a list of all the move related edges *)
let get_move_related_edges (graph: interfere_graph) : igedge list =
    let move_related_list : ignode list = igraph_filter_elements graph (fun n -> not (IGMoveSet.is_empty n.moves)) in
    (* List of all the move related edges in the graph *)
     List.fold_left
        (fun accum_list node -> (IGMoveSet.elements node.moves) @ accum_list) 
        [] 
        move_related_list

let rec coalesce (initial_state: reduction_state) : reduction_state =

    let move_edge_list = get_move_related_edges initial_state.reduce_igraph in
                                             
    let rec loop_worklist (edgelist: igedge list) : reduction_state =
        match edgelist with
            (* Empty list means no nodes in work list could be coaleasced *)
            | [] -> initial_state
            | edge::edgelist_tail ->
                  if (are_coalescable initial_state.reduce_igraph initial_state.register_count edge.node_var edge.interfere_var)
                  then 
                      (* Coalesce nodes in graph *)
                      let coalesced_graph = coalesce_nodes edge.node_var edge.interfere_var initial_state.reduce_igraph in
                      (* Re-simplify graph and coalesce new graph from the beginning *)
                          coalesce (simplify (reduction_set_igraph coalesced_graph initial_state))
                  else 
                      (* Nodes are not coalesable, so move to next node in list *)
                      loop_worklist edgelist_tail 
    in
        loop_worklist move_edge_list

let rec generic_freeze (freeze_picker : interfere_graph -> igedge list -> igedge) (initial_state: reduction_state) : reduction_state =
    let move_edge_list = get_move_related_edges initial_state.reduce_igraph in
        if move_edge_list = []
            (* No freezing posible because no move related edges *)
        then 
            initial_state
        else 
            (* Pick an edge to remove *)
            let remove_edge = freeze_picker initial_state.reduce_igraph move_edge_list in
            (* Remove edge from graph *)
            let frozen_graph = remove_move_edge remove_edge.node_var remove_edge.interfere_var initial_state.reduce_igraph in
                (* Re-simplify, re-coalesce, and re-freeze *)
                generic_freeze freeze_picker (coalesce (simplify (reduction_set_igraph frozen_graph initial_state)))

(*  Freezes first node *)
let simple_freeze_picker (g: interfere_graph) (l: igedge list) : igedge =
    List.hd l 

let freeze = generic_freeze simple_freeze_picker

(* Spill functions *)

(* Mark a node for spilling *)
let rec mark_spill (initial_state: reduction_state) : reduction_state =
    let spill_picker (target : interfere_graph) : (ignode * interfere_graph) option =
        choose_node target
    in
    if IGNodeSet.empty = initial_state.reduce_igraph
    then 
        initial_state
    else 
        (* Pick an node to remove *)         (* Remove node from graph *)
        let picked = spill_picker initial_state.reduce_igraph in
        match picked with 
        | None                           -> initial_state
        | Some((target_node, new_graph)) -> 
            let new_stack = push_spill_node target_node initial_state.var_stack     in
            let new_state = (reduction_set_var_stack new_stack initial_state)       in
            let new_state = (reduction_set_igraph new_graph new_state)              in
                (* Re-simplify, re-coalesce, and re-freeze *)
                mark_spill (freeze (coalesce (simplify new_state)))

let spill state =
    state

(* Coloring functions *)

exception NoColors

let gen_register_list (register_count : int) : int list =
    let rec gen_r_list count_left accumulated = 
        if count_left < 0 
        then accumulated 
        else gen_r_list (count_left - 1) (count_left::accumulated)
    in 
    gen_r_list register_count [] 

let get_available_colors (neighbors : ignode list) 
                         (registers : int list) : int list = 
    let rec get_a_cols rem_neighbors rem_regs =
        match rem_neighbors with 
        | []     -> rem_regs
        | hd::tl -> let ocolor = hd.color in
                    (match ocolor with 
                    | None -> get_a_cols tl rem_regs
                    | Some(color) -> get_a_cols tl 
                                         (List.filter 
                                              (fun elt -> not (elt = color))
                                         rem_regs))
    in
    get_a_cols neighbors registers
    

let apply_color (color : int) (target : ignode) (state : reduction_state) : reduction_state =
    let new_node  = ignode_set_color (Some(color)) target      in
    let new_graph = update_igraph new_node state.reduce_igraph in
    reduction_set_igraph new_graph state

let color_single (node : ignode) (state : reduction_state) : reduction_state =
    (*  Calculate available colors *)
    let available_colors = get_available_colors 
                                                (get_neighbors node state.reduce_igraph) 
                                                (gen_register_list state.register_count) in
    match available_colors with
    | []     -> raise NoColors
    | hd::tl -> (apply_color hd node state)

let color_with_spill (node : ignode) (state : reduction_state) =
    let available_colors = get_available_colors 
                                                (get_neighbors node state.reduce_igraph) 
                                                (gen_register_list state.register_count) in
    match available_colors with
    | []     -> (spill state)
    | hd::tl -> (apply_color hd node state)

(* Coloring: *)
let rec color_graph (initial_state: reduction_state) : reduction_state =
    (* Pop stack *)
    let popped = pop_var_stack initial_state.var_stack in
    match popped with
    | None                           -> initial_state
    | Some((target_var, new_stack))  -> 
    let new_state = reduction_set_var_stack new_stack initial_state in
    let colored_state = 
        (match target_var with
        | Single(var_name)          -> (color_single (get_node var_name new_state.reduce_igraph) new_state)
        | Coalesced(var_list)       -> color_single (get_node_alias var_list new_state.reduce_igraph) new_state
        | Spill(var_name)           -> color_with_spill (get_node var_name new_state.reduce_igraph) new_state
        | Spill_Coalesced(var_list) -> color_with_spill (get_node_alias var_list new_state.reduce_igraph) new_state)
    in color_graph colored_state

(* Grab the actual node*)
(*  Calculate available colors *)
(*  Choose from available colors *)
(*  If no available colors & spill node, spill *)


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

(* Freeze *)
            (* Make a work list of move-related nodes *)
            (* Run through work list *)
                (* Pick a move_related edge to remove - this should be a function that we can swap in and out*)
                (*Remove that move related edge *)
                (* Resimplify, recoalesce, and freeze again if necessary *)
            (* Return state if worklist is empty *)
