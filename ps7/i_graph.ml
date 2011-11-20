type IGEdge = { left  : var;
                right : var; 
              } 

(* Sort based on left edge, but prevents duplication *)
module IGEdgeSet = Set.Make(struct 
                              type t = IGEdge 
                              let compare = 
                                fun e1 e2 ->
                                  let l_comp = String.compare e1.left e2.left   in
                                  let r_comp = String.compare e1.right e2.right in 
                                  if (lcomp = 0 && r_comp = 0) then 0 
                                  else l_comp
                            end)

module IGMoveSet = IGEdgeSet

type IGNode = { name  : var        ; 
                edges : IGEdgeSet.t;
                moves : IGMoveSet.t;
                color : int option ;
              } 

module IGNodeSet = Set.Make(struct 
                              type t = IGEdge 
                              let compare = 
                                fun e1 e2 ->
                                  String.compare e1.name e2.name 
                            end)

(* an interference graph maps a variable x to the set of variables that
 * y such that x and y are live at the same point in time.  It's up to
 * you how you want to represent the graph.  I've just put in a dummy
 * definition for now.  *)
type interfere_graph = IGNodeSet.t
