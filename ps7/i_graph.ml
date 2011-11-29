open Cfg_ast
open Io_types
exception TODO

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

(* Build interference graph *)

(* Helper - adds e to graph if not already present *)
let add_var (e: var) (graph: interfere_graph) : interfere_graph = 
    (* Set.add will return graph unchanged if e is already a member *)
    IGNodeSet.add (new_ignode e) graph

(* Adds variables in s to graph if not already present *)
let add_vars (s: VarSet.t) (graph: interfere_graph) : interfere_graph = 
    VarSet.fold add_var s graph 

(* Updates node for v to reflect that it conflicts with e. Does not change e's node to reflect that v interferences with e *)
let mark_interfere (v: var) (e: var) (graph: interfere_graph) : interfere_graph = raise TODO
    (* let v_node = get_node v graph *)

(* Sets e as conflicting with all the variables in s in graph *)
let mark_interferes (e: var) (s: VarSet.t) (graph: interfere_graph) : interfere_graph = 


raise TODO

(* Marks all variables in s as conflicting with each other in graph *)
let mark_set_interfere (s: VarSet.t) (graph: interfere_graph) : interfere_graph = raise TODO

(* Fold over blocks *)

(* FOREACH BLOCK *)

(* Mark all variables in block In set as conflicting *)
(* Mark all variables in block Out set as conflicting *)

(* Intersect In and Out sets *)

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
