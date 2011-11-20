open Io_types
open Cfg_ast

let get_rw (i: inst) : io_inst =
  { inst_read = ReadSet.empty; inst_write = WriteSet.empty; inst_in = InSet.empty; inst_out = OutSet.empty; inst_move = []; src_inst = i}
