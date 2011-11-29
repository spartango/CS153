open Cfg_ast
open Io_types
open Cfg_gen
exception TODO

(* Why do we have this left/right edge piece. Can an edge set simply be a var set of the variables the node's variable conflicts with? *)
type igedge = { left  : var;
                right : var; 
              } 

(* Sort based on left edge, but prevents duplication *)
module IGEdgeSet = Set.Make(struct 
                              type t = igedge 
                              let compare = 
                                fun e1 e2 ->
                                  let l_comp = String.compare e1.left e2.left   in
                                  let r_comp = String.compare e1.right e2.right in 
                                  if (l_comp = 0 && r_comp = 0) then 0 
                                  else l_comp
                            end)

module IGMoveSet = IGEdgeSet

type ignode = { name  : var         ; 
                edges : IGEdgeSet.t ;
                moves : IGMoveSet.t ;
                color : int option  ;
              } 

module IGNodeSet = Set.Make(struct 
                              type t = ignode 
                              let compare = 
                                fun e1 e2 ->
                                  String.compare e1.name e2.name 
                            end)

let new_ignode (v: var) : ignode =
    { name = v                ;
      edges = IGEdgeSet.empty ;
      moves = IGEdgeSet.empty ;
      color = None            ;
    }

let ignode_set_edges (edgeset: IGEdgeSet.t) (n: ignode) : ignode =
    { name = n.name   ;
      edges = edgeset ;
      moves = n.moves ;
      color = n.color ;
    }

let ignode_set_moves (moveset: IGMoveSet.t) (n: ignode) : ignode =
    { name = n.name   ;
      edges = n.edges ;
      moves = moveset ;
      color = n.color ;
    }

let ignode_set_color (color: int option) (n: ignode) : ignode =
    { name = n.name   ;
      edges = n.edges ;
      moves = n.moves ;
      color = color   ;
    }

(* an interference graph maps a variable x to the set of variables that
 * y such that x and y are live at the same point in time.  It's up to
 * you how you want to represent the graph.  I've just put in a dummy
 * definition for now.  *)
type interfere_graph = IGNodeSet.t

let get_node (v : var) (target : interfere_graph) : ignode = 
  let filtered = IGNodeSet.filter (fun node -> node.name = v) target in
  IGNodeSet.choose filtered 

(* Updates graph with new copy of node *)
let update_graph (n: ignode) (graph: interfere_graph) : interfere_graph =
    (* Remove node from graph - nodes compared by name *)
    let graph1 = IGNodeSet.remove n graph in
    (* Add node back into graph *)
        IGNodeSet.add n graph1

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
    let updated_edges = IGEdgeSet.add {left = e; right = v} v_node.edges in
    let updated_node = ignode_set_edges updated_edges v_node in
        update_graph updated_node graph 

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
graph2

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
            add_interfere_set i.inst_out igraph1 in

    (* Adds a single variable to a graph and marks In/Out sets as interfering *)
    let var_io_interfere (v: var) : interfere_graph =
        let igraph1 = add_var v graph in
            io_interfere igraph1 in
        (* Match on inst to see if it defines a var *)
        match i.src_inst with
            | Move(Var x, _) -> var_io_interfere x
            | Arith(Var x, _, _, _) -> var_io_interfere x
            | Load(Var x, _, _) -> var_io_interfere x
            | _ -> io_interfere graph

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


let build_block_igraph (b: io_block) : interfere_graph =
    (* inst_gen_io_base is a more general version of inst_gen_io that allows you to specify the base out set *)
    let updated_insts = inst_gen_io_base b.block_out b.insts in
        List.fold_left add_inst_interferes IGNodeSet.empty updated_insts




(* OLD ALGORITHM *)

(*
    
    let igraph1 = add_interfere_set b.block_in IGNodeSet.empty in
    let igraph2 = add_interfere_set b.block_out igraph1 in
    (* Get intersections of In/Out sets *)
    let io_intersect = VarSet.inter b.block_in b.block_out in
        igraph2
*)

(* FOREACH BLOCK *)

(* Add block In/Out variables to graph and mark as interfering with themselves *)
(* Get intersection of In/Out sets *)

(* For each instruction in a block *)
      (* Check if variable whose value is assigned in instruction is in graph 
       * - Add if not in graph
       * - Ignore if/jump/return - i.e. ignore control flow instructions that terminate blocks*)

      (* Mark all variables in In Set as conflicting with each other in graph 
       * - Fold over In set
       * - Pass removed element to mark_set_conflict with the complete In set minus the remove element *)
      (* Mark all variables in Out set as conflicting with each other - same procedure as In set *)

(* Mark variables in the resulting union as interfering with all variables in the block *)

(* Merge graph with accumulated i_graph *)

(* END FOREACH *)  

(* Mark move related variables and unmark them as interferening *)
