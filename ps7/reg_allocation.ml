open Cfg_ast
open Io_types
open I_graph
open Stack
open Utility

exception Exceed_max_regs
exception Uncolored_node

(* Debug tools *)
let print_graph (g: interfere_graph) (l: string) : unit =
    print_endline (l ^ ":\n" ^ (igraph2str g))

let print_var (v: var) (l: string) : unit =
    print_endline (l ^ ": " ^ v ^ "\n")

let print_node (n: ignode) (l: string) : unit =
    print_endline (l ^ ":\n" ^ (ignode2str n))

(* Container for passing around a graph and the var stack associated with it *)
type reduction_state = {colored_igraph: interfere_graph; reduce_igraph : interfere_graph; var_stack : VarStack.t; register_count : int; initial_func : func;}

(* Makes a new reduction state with *)
let mk_reduction_state (initial_g: interfere_graph) (reduce_g: interfere_graph) (v_stack: VarStack.t) (regs: int) (f: func) : reduction_state = 
    {
        colored_igraph = initial_g;
        reduce_igraph = reduce_g;
        var_stack = v_stack;
        register_count = regs;
        initial_func = f;
    }

let empty_reduction_state = mk_reduction_state IGNodeSet.empty IGNodeSet.empty VarStack.empty 0 []
;;

let reduction_set_igraph (graph: interfere_graph) (rs: reduction_state) : reduction_state =
    {
        colored_igraph = rs.colored_igraph;
        reduce_igraph = graph;
        var_stack = rs.var_stack;
        register_count = rs.register_count;
        initial_func = rs.initial_func;
    }
let reduction_set_var_stack (v_stack : VarStack.t) (rs: reduction_state) : reduction_state =
    {
        colored_igraph = rs.colored_igraph;
        reduce_igraph = rs.reduce_igraph;
        var_stack = v_stack;
        register_count = rs.register_count;
        initial_func = rs.initial_func;
    }
let reduction_set_register_count (regs : int) (rs: reduction_state) : reduction_state =
    {
        colored_igraph = rs.colored_igraph;
        reduce_igraph = rs.reduce_igraph;
        var_stack = rs.var_stack;
        register_count = regs;
        initial_func = rs.initial_func;
    }

let reduction_set_initial_func (f : func) (rs: reduction_state) : reduction_state =
    {
        colored_igraph = rs.colored_igraph;
        reduce_igraph = rs.reduce_igraph;
        var_stack = rs.var_stack;
        register_count = rs.register_count;
        initial_func = f;
    }

let reduction_set_colored_igraph (g: interfere_graph) (rs: reduction_state): reduction_state =
    {
        colored_igraph = g;
        reduce_igraph = rs.reduce_igraph;
        var_stack = rs.var_stack;
        register_count = rs.register_count;
        initial_func = rs.initial_func;
    }

let initial_reduction_state (graph: interfere_graph) (regs: int) (f: func) : reduction_state =
    reduction_set_initial_func f (reduction_set_register_count regs (reduction_set_igraph graph (reduction_set_colored_igraph graph empty_reduction_state)))

exception Implement_Me

(* Helper functions *)

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
    let _ = print_endline "Simplify" in
    (* Performs one round of simplification over the graph *)
    let rec simplify_r (reduce_state: reduction_state) (worklist: ignode list) : reduction_state =
        match worklist with 
            | [] -> reduce_state
            | n::worklist_tail ->
                  if (is_simplifiable n initial_state.register_count)
                  then 
                      let new_stack = push_node n reduce_state.var_stack in
                      let new_graph = remove_node n reduce_state.reduce_igraph in
                          simplify_r (
                              reduction_set_igraph new_graph (reduction_set_var_stack new_stack reduce_state)) worklist_tail
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
    let related_node = get_node related_var graph in
    (* Do not coalesce if two nodes interfere or the other node is colored *)
    if IGEdgeSet.mem {node_var = node_v; interfere_var = related_var} node.edges || (is_colored related_node)
    then
            false
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
    let combined_color = if not (node.color = None) then node.color else combined_node.color in
    let combined_coalesced =
        match (node.coalesced, combined_node.coalesced) with
        | (None, None) -> Some([combined_node.name])
        | (Some(c), None) -> Some(combined_node.name::c)
        | (None, Some(c)) -> Some(combined_node.name::c)
        | (Some(c1), Some(c2)) -> Some(combined_node.name::(c1 @ c2)) in
        ignode_set_color combined_color (ignode_set_coalesced combined_coalesced (ignode_set_moves combined_moves (ignode_set_edges combined_edges node)))

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
    let coalesced_state = loop_worklist move_edge_list in
    let _ = print_graph coalesced_state.reduce_igraph "Coalesced graph" in
        coalesced_state

    (* Does not matter what output of loop_worklist is, as if it reaches this point, is simply initial_state.reduce_igraph *)
(*
    let rec loop_move_interfere_related (edgelist: igedge list) (coalesced_state: reduction_state) : reduction_state =
        match edgelist with
            | [] -> coalesced_state (* No both move related and interfere related edges to remove *)
            | edge::edgelist_tail ->
                  (* Get node out of graph *)
                  let node = get_node edge.node_var coalesced_state.reduce_igraph in
                      (* Check if nodes are both move and interfere_related *)
                      if IGEdgeSet.mem {node_var = node.name; interfere_var = edge.interfere_var} node.edges 
                      then 
                          (* Get move related node *)
                          let move_related = get_node edge.interfere_var coalesced_state.reduce_igraph in
                              (* Remove interference edge from graph *)
                          let node = remove_interfere node edge.interfere_var in
                          let move_related = remove_interfere move_related edge.node_var in
                          (* Update graph *)
                          let updated_igraph = update_igraph node (update_igraph move_related coalesced_state.reduce_igraph) in
                              coalesce (simplify (reduction_set_igraph updated_igraph coalesced_state))
                      else 
                          loop_move_interfere_related edgelist_tail coalesced_state in
       
    let coalesced_state = loop_worklist move_edge_list in
    let coalesced_move_edge_list = get_move_related_edges coalesced_state.reduce_igraph in
        loop_move_interfere_related coalesced_move_edge_list coalesced_state
*)
                                          
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
    if IGNodeSet.empty = initial_state.reduce_igraph || IGNodeSet.for_all (fun n -> is_colored n) initial_state.reduce_igraph
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

let op_contains_var var_name op =
    match op with
    | Var(name) -> var_name = name
    | _         -> false

let contains_var_read var_name t_inst =
    match t_inst with 
    | Move(_, op)           -> (op_contains_var var_name op)
    | Arith(_, op, _,  op2) -> (op_contains_var var_name op) || (op_contains_var var_name op2)
    | Load(_, op, _)        -> (op_contains_var var_name op)
    | Store(op, _, op2)     -> (op_contains_var var_name op) || (op_contains_var var_name op2)
    | Call(op)              -> (op_contains_var var_name op)
    | Jump(_)               -> false
    | If(op, _, op2, _, _)  -> (op_contains_var var_name op) || (op_contains_var var_name op2)
    | Label(_)              -> false
    | Return                -> false

let contains_var_write var_name t_inst =
    match t_inst with 
    | Move(op, _)           -> (op_contains_var var_name op)
    | Arith(op, _, _, _)    -> (op_contains_var var_name op) 
    | Load(op, _, _)        -> (op_contains_var var_name op)
    | Store(_, _, _)        -> false
    | Call(_)               -> false
    | Jump(_)               -> false
    | If(_, _, _, _, _)     -> false
    | Label(_)              -> false
    | Return                -> false
let spill_offset = ref 0;;
 (* let spill_map    = ref VarMap.empty;; *)

let spill node state = 
    let var_name        = node.name                            in
    let fn_body         = state.initial_func                   in
    (* Allocate an offset *)              
    let var_offset      = !spill_offset                        in
    let _ = (spill_offset := !(spill_offset) + 4)              in
    let first_block = (List.hd fn_body)                        in
    let label_inst  = (List.hd first_block)                    in
    let first_block = Arith(sp, sp, Plus, Int(4))::(List.tl first_block) in
    let first_block = label_inst::first_block in
    let new_blocks  = first_block::(List.tl fn_body)           in
    (* Trawl through the blocks *)
    let new_blocks = List.map 
        (fun t_block ->
            (* Trawl through the instructions; HOTSPOT OOO *)
            List.fold_left 
                (fun insts t_inst ->
                    if (contains_var_read var_name t_inst) 
                    then
                    (* If theres a read of the var *)
                    (* Append a load ahead of it*) 
                    insts @ [Load(Var(var_name), fp, var_offset); t_inst]
                    else if (contains_var_write var_name t_inst)
                    then 
                    (* If theres a write, Append a store after it*)
                    insts @ [t_inst; Store(fp, var_offset, Var(var_name)) ]
                    else insts @ [t_inst]
                )
                []
                t_block
        ) new_blocks
    in
    reduction_set_initial_func new_blocks state


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
    let new_graph = update_igraph new_node state.colored_igraph in
        reduction_set_colored_igraph new_graph state

let color_single (node : ignode) (state : reduction_state) : reduction_state =
    (*  Calculate available colors *)
    let available_colors = get_available_colors 
                                                (get_neighbors node state.colored_igraph) 
                                                (gen_register_list state.register_count) in
    match available_colors with
    | []     -> raise NoColors
    | hd::tl -> (apply_color hd node state)

let color_with_spill (node : ignode) (state : reduction_state) =
    let available_colors = get_available_colors 
                                                (get_neighbors node state.colored_igraph) 
                                                (gen_register_list state.register_count) in
    match available_colors with
    | []     -> (spill node state)
    | hd::tl -> (apply_color hd node state)

(* Coloring: *)
let rec color_graph (initial_state: reduction_state) : reduction_state =
    let _ = print_graph initial_state.reduce_igraph "Reduced Igraph" in
    (* Pop stack *)
    let popped = pop_var_stack initial_state.var_stack in
    match popped with
    | None                           -> initial_state
    | Some((target_var, new_stack))  -> 
    let new_state = reduction_set_var_stack new_stack initial_state in
    let colored_state = 
        (match target_var with
        | Single(var_name)          -> 
              let _ = print_endline var_name in
(color_single (get_node var_name new_state.colored_igraph) new_state)
        | Coalesced(var_list)       -> color_single (get_node_alias var_list new_state.colored_igraph) new_state
        | Spill(var_name)           -> color_with_spill (get_node var_name new_state.colored_igraph) new_state
        | Spill_Coalesced(var_list) -> color_with_spill (get_node_alias var_list new_state.colored_igraph) new_state)
    in color_graph colored_state

let lookup_color (v: var) (graph: interfere_graph) : int =
    let node = get_node v graph in
        match node.color with
            | None ->
                  let _ = print_endline v in
                      raise Uncolored_node
            | Some(color) -> color

(* Assume we have no more than than 24 register to mess with *)
let build_index (r: reduction_state) : Mips.reg VarMap.t =
    if r.register_count > 24
    then 
        raise Exceed_max_regs
    else
        let pass1 = IGNodeSet.fold (fun node index ->
                            let color = lookup_color node.name r.colored_igraph in
                            (* Register will be color number + 2 *)
                            let reg = Mips.str2reg ("$" ^ string_of_int (color + 2)) in
                            let _ = print_endline (Mips.reg2string reg) in
                                VarMap.add node.name reg index)
            r.colored_igraph
                            VarMap.empty in
        IGNodeSet.fold (fun node index ->
                            match node.coalesced with
                                | None -> index
                                | Some coal ->
                                      let reg = lookup_color node.name index in
                                          List.fold_left (fun accum n ->
                                                              VarMap.add n reg accum) index coal
                                

exception Node_not_in_graph

let lookup_assigned_reg (v: var) (index: Mips.reg VarMap.t) : Mips.reg =
   try
       VarMap.find v index
   with
           _ ->
               let _ = print_endline v in

                   raise Node_not_in_graph

(* Rewrite all vars with their assigned register *)
let rewrite_operand (o: operand) (index: Mips.reg VarMap.t) : operand =
    match o with
        (* Return the register x is assigned to *)
        | Var x -> Reg(lookup_assigned_reg x index)
        | Int i -> Int(i)
        | Reg r -> Reg(r)
        | Lab l -> Lab(l)                 

let rewrite_inst (i: inst) (index: Mips.reg VarMap.t) : inst =
    let rewrite = (fun o -> rewrite_operand o index) in
    match i with
        | Label l -> Label l
        (* Be conservative *)
        | Move (o1, o2) ->
              Move(rewrite o1, rewrite o2)
        | Arith(o1, o2, c, o3) -> 
              Arith(rewrite o1, rewrite o2, c, rewrite o3)
        | Load(o1, o2, i) ->
              Load(rewrite o1, rewrite o2, i)
        | Store(o1, i, o2) ->
              Store(rewrite o1, i, rewrite o2)
        | Call(o) ->
              Call (rewrite o)
        | Jump(l) ->
              Jump(l)
        | If(o1, c, o2, l1, l2) ->
              If(rewrite o1, c, rewrite o2, l1, l2)
        | Return ->
              Return

let rewrite_block (b: block) (index: Mips.reg VarMap.t) : block =
    List.fold_right (fun i accumulated ->
                         match i with
                             (* Remove stupid moves! *)
                             | Move (o1, o2) ->
                                   let r1 = rewrite_operand o1 index in
                                   let r2 = rewrite_operand o2 index in
                                       if r1 = r2
                                       then accumulated
                                       else (Move(r1, r2))::accumulated
                             | _ -> (rewrite_inst i index)::accumulated)
        b
        []
                                   
let rewrite_code (colored_state: reduction_state) : func =
    (* Build index of colors to registers - reserve $0, $1, $26 - $31 *)
    let var_index = build_index colored_state in
        List.map (fun b -> rewrite_block b var_index) colored_state.initial_func

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
