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

let io_inst_set_read readset target = 
  { inst_read  = readset;
    inst_write = target.inst_write;
    inst_in    = target.inst_in;
    inst_out   = target.inst_out;
    inst_move  = target.inst_move;
    src_inst   = target.src_inst;
  }

let io_inst_set_write writeset target = 
  { inst_read  = target.inst_read;
    inst_write = writeset;
    inst_in    = target.inst_in;
    inst_out   = target.inst_out;
    inst_move  = target.inst_move;
    src_inst   = target.src_inst;
  }

let io_inst_set_in inset target = 
  { inst_read  = target.inst_read;
    inst_write = target.inst_write;
    inst_in    = inset;
    inst_out   = target.inst_out;
    inst_move  = target.inst_move;
    src_inst   = target.src_inst;
  }

let io_inst_set_out inset target = 
  { inst_read  = target.inst_read;
    inst_write = target.inst_write;
    inst_in    = target.inst_in;
    inst_out   = outset;
    inst_move  = target.inst_move;
    src_inst   = target.src_inst;
  }

let io_inst_set_move move target = 
  { inst_read  = target.inst_read;
    inst_write = target.inst_write;
    inst_in    = target.inst_in;
    inst_out   = target.inst_out;
    inst_move  = move;
    src_inst   = target.src_inst;
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

let io_block_set_in t target : block = 
  { block_in     = t;
    block_out    = target.block_out;
    block_move   = target.block_move;
    master_read  = target.master_read;
    master_write = target.master_write;
    insts        = target.insts;
    src_block    = target.src_block;
  }

let io_block_set_out t target : block = 
  { block_in     = target.block_in;
    block_out    = t;
    block_move   = target.block_move;
    master_read  = target.master_read;
    master_write = target.master_write;
    insts        = target.insts;
    src_block    = target.src_block;
  }

let io_block_set_move t target : block = 
  { block_in     = target.block_in;
    block_out    = target.block_out;
    block_move   = t;
    master_read  = target.master_read;
    master_write = target.master_write;
    insts        = target.insts;
    src_block    = target.src_block;
  }

let io_block_set_read t target : block = 
  { block_in     = target.block_in;
    block_out    = target.block_out;
    block_move   = target.block_move;
    master_read  = t;
    master_write = target.master_write;
    insts        = target.insts;
    src_block    = target.src_block;
  }

let io_block_set_write t target : block = 
  { block_in     = target.block_in;
    block_out    = target.block_out;
    block_move   = target.block_move;
    master_read  = target.master_read;
    master_write = t;
    insts        = target.insts;
    src_block    = target.src_block;
  }

  let io_block_set_insts t target : block = 
  { block_in     = target.block_in;
    block_out    = target.block_out;
    block_move   = target.block_move;
    master_read  = target.master_read;
    master_write = target.master_write;
    insts        = t;
    src_block    = target.src_block;
  }