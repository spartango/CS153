open Cfg_ast
open Io_types
open Cfg_gen
open Pretty_print
exception TODO

(* node_var: Var of the node the edge belongs to 
   interfere_var Var interfering with node_var *)
type igedge = { interfere_var  : var;
                node_var       : var; 
              } 

let igedge2str (e: igedge) : string = 
        "(" ^ e.interfere_var ^ ", " ^ e.node_var ^ ")"

(* Sort based on interfere_var edge, but prevents duplication *)
module IGEdgeSet = Set.Make(struct 
                              type t = igedge 
                              let compare = 
                                fun e1 e2 ->
                                  let l_comp = String.compare e1.interfere_var e2.interfere_var   in
                                  let r_comp = String.compare e1.node_var e2.node_var in 
                                  if (l_comp = 0 && r_comp = 0) then 0 
                                  else l_comp
                            end)

let igedgeset2str (es: IGEdgeSet.t) : string =
    IGEdgeSet.fold (fun e str -> str ^ " " ^ (igedge2str e)) es ""

module IGMoveSet = IGEdgeSet


type ignode = { name  : var         ; 
                edges : IGEdgeSet.t ;
                moves : IGMoveSet.t ;
                color : int option  ;
                coalesced : (var list) option ;
              } 

module IGNodeSet = Set.Make(struct 
                              type t = ignode 
                              let compare = 
                                fun e1 e2 ->
                                  String.compare e1.name e2.name 
                            end)


let color2str (c: int option) : string =
    match c with
        | Some i -> (string_of_int i)
        | None -> "None"

let coalesced2str (c: var list option) : string =
    match c with 
        | Some vs -> List.fold_left (fun s v -> s ^ " " ^ v) "" vs
        | None -> "None"

let equal_coalesced (c1: var list option) (c2: var list option) : bool =
    match (c1,c2) with
        | (None, None) -> true
        | (Some(vs1), Some(vs2)) ->
              equal_lists vs1 vs2 String.compare (=)
        | (_,_) -> false

let ignode2str (i: ignode) : string = 
    "{" ^ 
        (format_string "\tName:\t "  Bright Cyan)^ i.name ^"\n" ^
        (format_string "\tEdges:\t " Bright Cyan) ^ (igedgeset2str i.edges) ^ "\n" ^
        (format_string "\tMoves:\t " Bright Cyan) ^ (igedgeset2str i.moves) ^ "\n" ^
        (format_string "\tColor:\t " Bright Cyan) ^ (color2str i.color) ^ "\n" ^
        (format_string "\tCoalesced:\t " Bright Cyan) ^ (coalesced2str i.coalesced) ^ "\n" ^
        "\n}\n"

let new_ignode (v: var) : ignode =
    { name  = v               ;
      edges = IGEdgeSet.empty ;
      moves = IGEdgeSet.empty ;
      color = None            ;
      coalesced = None        ;
    }

let ignode_set_edges (edgeset: IGEdgeSet.t) (n: ignode) : ignode =
    { name  = n.name  ;
      edges = edgeset ;
      moves = n.moves ;
      color = n.color ;
      coalesced = n.coalesced ;
    }

let ignode_set_moves (moveset: IGMoveSet.t) (n: ignode) : ignode =
    { name  = n.name  ;
      edges = n.edges ;
      moves = moveset ;
      color = n.color ;
      coalesced = n.coalesced ;
    }

let ignode_set_color (color: int option) (n: ignode) : ignode =
    { name  = n.name  ;
      edges = n.edges ;
      moves = n.moves ;
      color = color   ;
      coalesced = n.coalesced ;
    }

let ignode_set_coalesced (coalesced: var list option) (n: ignode) : ignode =
     { name  = n.name  ;
      edges = n.edges ;
      moves = n.moves ;
      color = n.color   ;
      coalesced = coalesced ;
    }   

(* an interference graph maps a variable x to the set of variables that
 * y such that x and y are live at the same point in time.  It's up to
 * you how you want to represent the graph.  I've just put in a dummy
 * definition for now.  *)
type interfere_graph = IGNodeSet.t

let igraph2str (ig: interfere_graph) : string = 
    IGNodeSet.fold (fun n str -> str ^ (ignode2str n)) ig ""

let build_edge_set (v: var) (edge_vars: var list) : IGEdgeSet.t =
    List.fold_left (fun set edge_var  -> IGEdgeSet.add { interfere_var = edge_var; node_var = v} set) IGEdgeSet.empty edge_vars

let build_node (v: var) (interferes: var list) : ignode =
    let node1 = new_ignode v in
        ignode_set_edges (build_edge_set v interferes) node1

let build_graph (nodes: ignode list) : interfere_graph =
    List.fold_left (fun g n -> IGNodeSet.add n g) IGNodeSet.empty nodes

let equal_nodes (n1: ignode) (n2: ignode) : bool =
    n1.name = n2.name &&
    IGEdgeSet.equal n1.edges n2.edges &&
    IGMoveSet.equal n1.moves n2.moves &&
    n1.color = n2.color &&
    equal_coalesced n1.coalesced n2.coalesced

let get_node (v : var) (target : interfere_graph) : ignode = 
  let filtered = IGNodeSet.filter (fun node -> node.name = v) target in
  IGNodeSet.choose filtered 

(* Updates graph with new copy of node *)
let update_igraph (node: ignode) (graph: interfere_graph) : interfere_graph =
    (* Remove node from graph. Returns set of all nodes whose name is not n.name *)
    let graph1 = IGNodeSet.filter (fun n -> n.name <> node.name) graph in
    (* Add node back into graph *)
        IGNodeSet.add node graph1

(* Helper - adds e to graph if not already present *)
let add_var (e: var) (graph: interfere_graph) : interfere_graph = 
    (* Set.add will return graph unchanged if e is already a member *)
    IGNodeSet.add (new_ignode e) graph

(* Adds variables in s to graph if not already present *)
let add_vars (s: VarSet.t) (graph: interfere_graph) : interfere_graph = 
    VarSet.fold add_var s graph 

(* Updates node for v to reflect that it conflicts with e. Does not change e's node to reflect that v interferences with e *)
(* Assumes v is already in graph *)
let mark_interfere (v: var) (e: var) (graph: interfere_graph) : interfere_graph =
    let v_node = get_node v graph in
    (* New edge set - TODO CHECK whether this is the "right" idea with right/left edges?*)
    let updated_edges = IGEdgeSet.add {interfere_var = e; node_var = v} v_node.edges in
    let updated_node = ignode_set_edges updated_edges v_node in
        update_igraph updated_node graph 

(* Sets e as conflicting with all the variables in s in graph *)
let mark_interferes (s: VarSet.t) (base_var: var) (graph: interfere_graph) : interfere_graph = 
    VarSet.fold (fun interfere_var partial_graph ->
                     (* Check that a variable is not interfering with itself *)
                         if base_var = interfere_var
                         then partial_graph
                         else mark_interfere base_var interfere_var partial_graph) s graph 

(* Marks all variables in s as conflicting with each other in graph *)
let mark_set_interfere (s: VarSet.t) (graph: interfere_graph) : interfere_graph = 
    VarSet.fold (mark_interferes s) s graph

(* Adds set of variables s to graph and marks variables in s as conflicting with each other *)
let add_interfere_set (s: VarSet.t) (g: interfere_graph) : interfere_graph = 
    mark_set_interfere s (add_vars s g)

(* Merges two IGNodeSets
 * Returns the union of the two sets. When both sets contain the same member, unions their edge and move sets *)
(* WILL RESET COLOR to "None" *)
let igraph_merge (graph1: interfere_graph) (graph2: interfere_graph) : interfere_graph =

    let merge_node (node: ignode) ((g2, igraph): interfere_graph * interfere_graph) : interfere_graph * interfere_graph =
        (* If given node from graph1 is member of graph2 *)
        if (IGNodeSet.mem node g2)
        then 
            let g2_node = get_node node.name g2 in
            (* Create new node with union of edges and moves *)
            let new_node = ignode_set_edges (IGEdgeSet.union node.edges g2_node.edges) (ignode_set_moves (IGMoveSet.union node.moves g2_node.moves) node) in 
            (* Remove g2_node from g2 *)
            let reduced_g2 = IGNodeSet.remove g2_node g2 in
            (* Add new node to igraph under construction *)
            let new_igraph = IGNodeSet.add new_node igraph in
                (reduced_g2, new_igraph)
        else 
            (* Add node unchanged to new graph *)
            let new_igraph = IGNodeSet.add node igraph in
                (g2, new_igraph) in
        
    let (reduced_g2, new_igraph) = IGNodeSet.fold merge_node graph1 (graph2, IGNodeSet.empty) in
        (* Union remainder of graph2 with new_igraph. *)
        IGNodeSet.union new_igraph reduced_g2;;

(* CHANGED ALGORITHM 
 * Our original algorithm, at least according to my understanding, would not produce the correct result in this case:
 * a = 5 + 4
 * b = 7 + 2
 * c = b + 1
 * if c > 3 then L2 else L3
 * where the In set is {} and Out {a}
 * Our original algorithm would not mark a as conflicting with b and c
 * The new algorithm is simpler - recalculate the in and out sets for each instruction, except starting with the block Out set as the Out 
 * set of the last instruction in the list. Then mark the element in each in/out at the instruction level as conflicting with each other. *)


(* We could simplify this function if we didn't add the defined variable. That should
 * be ok to do, because if either the variable is read later, in which case it appears
 * in the out set, or it is not read later and should be removed *)
let add_inst_interferes (graph: interfere_graph) (i: io_inst) : interfere_graph =

    (* Helper - marks In/Out sets as interfering with themselves in the graph *)
    let io_interfere (g: interfere_graph) : interfere_graph =
        let igraph1 = add_interfere_set i.inst_in g in
            add_interfere_set i.inst_out igraph1 
    in
    (* Adds a single variable to a graph and marks In/Out sets as interfering *)
    let var_io_interfere (v: var) : interfere_graph =
        let igraph1 = add_var v graph in
            io_interfere igraph1 
    in
    (* Match on inst to see if it defines a var *)
    match i.src_inst with
        | Move(Var x, _)        
        | Arith(Var x, _, _, _) 
        | Load(Var x, _, _)      -> var_io_interfere x
        | _                      -> io_interfere graph
    
(* NEW ALGORITHM *)

(* Map over blocks *)

(* FOREACH BLOCK *)

(* Revise io_insts - recalculate inst IN/OUT sets starting with the block Out as the Out of the final insts *)

(* FOREACH INST *)

(* Add var to graph, if defines var *)
(* Add In set vars and mark as interfering with each other *)
(* Add Out set vars and mark as interfering with each other *)

(* END FOREACH INST *)

(* END FOREACH BLOCK *)


(* Merge block igraphs into one *)
(* Deal with move related variables *)

let make_move_edges (ms: move_related list) (graph: interfere_graph) : interfere_graph =
    List.fold_left (fun g mv -> 
                        let (var1, var2) = mv in
                        let node1 = get_node var1 g in
                        let move1 = { interfere_var = var2; node_var = var1 } in
                        let node2 = get_node var2 g in
                        let move2 = { interfere_var = var1; node_var = var2 } in
                        let node1_updated = ignode_set_moves (IGMoveSet.add move1 node1.moves) node1 in
                        let node2_updated = ignode_set_moves (IGMoveSet.add move2 node2.moves) node2 in
                            update_igraph node1_updated (update_igraph node2_updated g)) graph ms

let build_block_igraph (b: io_block) : interfere_graph =
    (* inst_gen_io_base is a more general version of inst_gen_io that allows you to specify the base out set *)
    let updated_insts = inst_gen_io_base b.block_out b.insts in
    let igraph1 = List.fold_left add_inst_interferes IGNodeSet.empty updated_insts in
        make_move_edges b.block_move igraph1
        

let build_igraph (bs: io_block list) : interfere_graph =
    List.fold_left (fun g1 b ->
                        igraph_merge g1 (build_block_igraph b)) IGNodeSet.empty bs

(* Gets a single element off the igraph. returns an tuple of a node and the remaining igraph or None if empty igraph *)
let choose_node (graph: interfere_graph) : (ignode * interfere_graph) option =
    try
        let node = IGNodeSet.choose graph in
            Some (node, (IGNodeSet.remove node graph))
    with
            Not_found -> None

(*
let equal_edge_set

let rec equal_igraph (g1: interfere_graph) (g2: interfere_graph) : bool =
    match (get_node g1, get_node g2) with
        | (None, None) ->
              true
        | (Some(n1, g1_remainder), Some(n2, g2_remainder)) ->
              if (n1.name = n2.name &&
                     IGVarSet.
                       
*)
