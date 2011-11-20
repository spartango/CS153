open Io_types
open Cfg_ast

let get_rw (i: inst) : io_inst =
  { inst_read = ReadSet.empty; inst_write = WriteSet.empty; inst_in = InSet.empty; inst_out = OutSet.empty; inst_move = []; src_inst = i}

let gen_in (out_set : OutSet.t) 
           (read    : ReadSet.t) 
           (write   : WriteSet.t) =
  let o_sub = (VarSet.inter (VarSet.diff out_set write) out_set) in
  (VarSet.union read o_sub)

let gen_out (child_in_sets : InSet.t list) = 
  List.fold_left VarSet.inter VarSet.empty child_in_sets
      
