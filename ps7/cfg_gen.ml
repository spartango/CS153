open Cfg_ast

module VarSet   = Set.Make(struct type t = var let compare = String.compare end)
module OutSet   = VarSet
module InSet    = VarSet
module ReadSet  = VarSet
module WriteSet = VarSet

type move_related = var * var
  
type io_inst  = { inst_read : ReadSet.t        ;
                  inst_write: WriteSet.t       ; 
                  inst_in   : InSet.t          ;
                  inst_out  : OutSet.t         ;
                  inst_move : move_related list;
                  src_inst  : inst             ;
                }

let new_io_inst src : inst = 
  { inst_read  = ReadSet.empty;
    inst_write = WriteSet.empty;
    inst_in    = InSet.empty;
    inst_out   = OutSet.empty;
    inst_move  = [];
    src_inst   = src;
  }

type io_block = { block_in    : InSet.t          ;
                  block_out   : OutSet.t         ;
                  block_move  : move_related list;
                  master_read : ReadSet.t        ;
                  master_write: WriteSet.t       ;
                  insts       : io_inst list     ;
                  src_block   : block            ;
                }

let new_io_block src : block = 
  { block_in     = InSet.empty;
    block_out    = OutSet.empty;
    block_move   = [];
    master_read  = ReadSet.empty;
    master_write = WriteSet.empty;
    insts        = [];
    src_block    = src;
  }