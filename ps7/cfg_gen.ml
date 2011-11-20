open Cfg_ast

module VarSet   = Set.Make(struct type t = var let compare = String.compare end)
module OutSet   = VarSet
module InSet    = VarSet
module ReadSet  = VarSet
module WriteSet = VarSet

type move_related = var * var
  
type io_inst  = { inst_in   : InSet.t          ;
                  inst_out  : OutSet.t         ;
                  inst_move : move_related list;
                  src_inst  : inst             ;
                }

type io_block = { block_in    : InSet.t          ;
                  block_out   : OutSet.t         ;
                  block_move  : move_related list;
                  master_read : ReadSet.t        ;
                  master_write: WriteSet.t       ;
                  insts       : io_inst list     ;
                  src_block   : block            ;
                }