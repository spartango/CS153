open Io_types
open Cfg_ast

let get_rw (i: inst) : io_inst =
    (* Builds a set from a list of operands *)
    let set_of_ops (os: operand list) : VarSet.t =
        List.fold_left (fun a e -> match e with 
                                Var x -> VarSet.add x a
                                    _ -> a) VarSet.empty os in
    let get_move_related (o1: operand) (o2: operand) : move_related = 
        match (o1, o2) with
            | (Var(x1),Var(x2) -> (x1, x2)
                   _ -> raise InvalidCFGCode
    let io_in = new_io_inst in
    match i with
        | Move(o1, o2) -> 
              
    
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





  { inst_read = ReadSet.empty; inst_write = WriteSet.empty; inst_in = InSet.empty; inst_out = OutSet.empty; inst_move = []; src_inst = i}
