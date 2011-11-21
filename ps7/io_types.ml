open Cfg_ast

exception InvalidCFGCode

module VarSet   = Set.Make(struct type t = var let compare = String.compare end)
module OutSet   = VarSet
module InSet    = VarSet
module ReadSet  = VarSet
module WriteSet = VarSet
module BlockSet = VarSet

let varset_add set elt =
  VarSet.add elt set

let set_add_all (elements : var list) target = 
  List.fold_left varset_add target elements

let set_map f set = 
  VarSet.fold 
    (fun elt accum ->
      let applied = (f elt) in
      (accum @ [applied;]))
    set
    []

type move_related = var * var

type io_inst  = { inst_read : ReadSet.t        ;
                  inst_write: WriteSet.t       ; 
                  inst_in   : InSet.t          ;
                  inst_out  : OutSet.t         ;
                  inst_move : move_related list;
                  src_inst  : inst             ;
                }

let strlist2str (ss: string list) : string =
    List.fold_left (fun s e ->
                        s ^ " " ^ e) 
        ""
        ss

let varset2str (s: VarSet.t) : string = 
    strlist2str (VarSet.elements s)

let moverelated2str (m: move_related) :string = 
                        let(v1, v2) = m in
                           "(" ^ v1 ^ "," ^ v2 ^ ")"

let mvrelatedlist2str (ml: move_related list) : string =  
    List.fold_left (fun s e -> s ^ (moverelated2str e) ^ " ") "" ml
    
let ioinst2str (io: io_inst) :string =
    "{\n" ^
        "Inst:\t\t" ^ (inst2string io.src_inst) ^ "\n" ^
        "Read set:\t" ^ (varset2str io.inst_read) ^ "\n" ^
        "Write set:\t" ^ (varset2str io.inst_write) ^ "\n" ^
        "In set:\t" ^ (varset2str io.inst_in) ^ "\n" ^
        "Out set:\t" ^ (varset2str io.inst_out) ^ "\n" ^
        "Move related:\t" ^ (mvrelatedlist2str io.inst_move) ^ "\n}\n"

let io_inst_equal (i1: io_inst) (i2: io_inst) : bool = 
    (VarSet.equal i1.inst_read i2.inst_read) &
        (VarSet.equal i1.inst_write i2.inst_write) & 
        (VarSet.equal i1.inst_in i2.inst_in) & 
        (VarSet.equal i1.inst_out i2.inst_out) & 
        (i1.src_inst = i2.src_inst) &
        (i1.inst_move = i2.inst_move)
 
let new_io_inst src : io_inst = 
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

let io_inst_set_out outset target = 
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

type io_block = { block_label : label            ;
                  block_in    : InSet.t          ;
                  block_out   : OutSet.t         ;
                  block_move  : move_related list;
                  master_read : ReadSet.t        ;
                  master_write: WriteSet.t       ;
                  insts       : io_inst list     ;
                  src_block   : block            ;
                  children    : BlockSet.t       ;
                }

let new_io_block src : io_block = 
  { block_label  = "";
    block_in     = InSet.empty;
    block_out    = OutSet.empty;
    block_move   = [];
    master_read  = ReadSet.empty;
    master_write = WriteSet.empty;
    insts        = [];
    src_block    = src;
    children     = BlockSet.empty;
  }

let io_block_set_in t target : io_block = 
  { block_label  = target.block_label;
    block_in     = t;
    block_out    = target.block_out;
    block_move   = target.block_move;
    master_read  = target.master_read;
    master_write = target.master_write;
    insts        = target.insts;
    src_block    = target.src_block;
    children     = target.children;
  }

let io_block_set_out t target : io_block = 
  { block_label  = target.block_label;
    block_in     = target.block_in;
    block_out    = t;
    block_move   = target.block_move;
    master_read  = target.master_read;
    master_write = target.master_write;
    insts        = target.insts;
    src_block    = target.src_block;
    children     = target.children;
  }

let io_block_set_move t target : io_block = 
  { block_label  = target.block_label;
    block_in     = target.block_in;
    block_out    = target.block_out;
    block_move   = t;
    master_read  = target.master_read;
    master_write = target.master_write;
    insts        = target.insts;
    src_block    = target.src_block;
    children     = target.children;
  }

let io_block_set_read t target : io_block = 
  { block_label  = target.block_label;
    block_in     = target.block_in;
    block_out    = target.block_out;
    block_move   = target.block_move;
    master_read  = t;
    master_write = target.master_write;
    insts        = target.insts;
    src_block    = target.src_block;
    children     = target.children;
  }

let io_block_set_write t target : io_block = 
  { block_label  = target.block_label;
    block_in     = target.block_in;
    block_out    = target.block_out;
    block_move   = target.block_move;
    master_read  = target.master_read;
    master_write = t;
    insts        = target.insts;
    src_block    = target.src_block;
    children     = target.children;
  }

let io_block_set_insts t target : io_block = 
  { block_label  = target.block_label;
    block_in     = target.block_in;
    block_out    = target.block_out;
    block_move   = target.block_move;
    master_read  = target.master_read;
    master_write = target.master_write;
    insts        = t;
    src_block    = target.src_block;
    children     = target.children;
  }

let io_block_set_label t target : io_block = 
  { block_label  = t;
    block_in     = target.block_in;
    block_out    = target.block_out;
    block_move   = target.block_move;
    master_read  = target.master_read;
    master_write = target.master_write;
    insts        = target.insts;
    src_block    = target.src_block;
    children     = target.children;
  }

let io_block_set_children t target : io_block = 
  { block_label  = target.block_label;
    block_in     = target.block_in;
    block_out    = target.block_out;
    block_move   = target.block_move;
    master_read  = target.master_read;
    master_write = target.master_write;
    insts        = target.insts;
    src_block    = target.src_block;
    children     = t;
  }


let lookup_block (name : label) (blocks : io_block list) =
   (* Assume uniqueness *)
   let search = List.filter (fun blk -> blk.block_label = name) blocks in
   (List.hd search)

