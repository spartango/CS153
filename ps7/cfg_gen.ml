open Io_types
open Cfg_ast

let get_rw (i: inst) : io_inst =
    (* Builds a set from a list of operands *)
    let set_of_ops (os: operand list) : VarSet.t =
        List.fold_right (fun e a -> match e with 
                            | Var x -> VarSet.add x a
                            | _ -> a) os VarSet.empty in
    let get_move_related (o1: operand) (o2: operand) : move_related = 
        match (o1, o2) with
            | (Var(x1),Var(x2)) -> (x1, x2)
            | _ -> raise InvalidCFGCode in 
    (* Builds a new io_inst record with reads rs and writes ws *)
    let set_rw (rs: operand list) (ws: operand list) = 
        io_inst_set_read (set_of_ops rs) (io_inst_set_write (set_of_ops ws) (new_io_inst i)) in
    match i with
        | Move(dest, src) -> io_inst_set_move [(get_move_related dest src)]
              (set_rw [src] [dest])
        | Arith(dest, s1, arthop, s2) ->
              set_rw [s1; s2] [dest]
        | Load(dest, adr, i) ->
              set_rw [adr] [dest]
        | Store(adr, i, src) ->
              set_rw [adr; src] []
        | Call(func_var) ->
              set_rw [func_var] []
        | Jump(l) ->
              set_rw [] []
        | Label l ->
              set_rw [] []
        | If(src1, compop, src2, lab1, lab2) ->
              set_rw [src1; src2] []
        | Return ->
              set_rw [] []
              
    
(*
  Move o1 o2 -> Write o1; Read o2
  Arith o1 o2 Arithop o3 -> Write o1; Read o2, o3
  Load o1 o2 Int -> Write o1; Read o2
  Store o1 Int o2 Write -; Read o1 o2
  Call o1 -> Write -; Read o1
  Jump l -> Write -; Read -
  If o1 Compareop o2 l1 l2 -> Write -; Read o1, o2
  Return -> Write -; Read -
*)
