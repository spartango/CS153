open Cfg_ast
open I_graph
open Io_types
open Cfg_gen
exception Implement_Me
exception FatalError

(*******************************************************************)
(* PS7 TODO:  interference graph construction *)


(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)
let build_interfere_graph (f : func) : interfere_graph =

    let build_io_block (b: block) : io_block =
        (* Generate empty io_block - leave ins, outs, and moves empty *)
        let io_block0 = new_io_block b in
        (* Get block's label *)
        let io_block1 = io_block_set_label (get_block_label b) io_block0 in
        (* Get block's children *)
        let io_block2 = io_block_set_children (get_block_children b) io_block1 in
        (* Build io_insts for block's instrucitons *)
        let rw_io_insts = List.map get_rw b in
        (* Build master read/write sets for block *)
        let (master_read, master_write) = 
                List.fold_left (fun accumulated io_rec ->
                                   let(read, writes) = accumulated in
                                       (ReadSet.union io_rec.inst_read, WriteSet.union io_rec.inst_write)) (ReadSet.empty, WriteSet.empty) rw_io_insts in
        (* Add master read/writes to io_block *)
        let io_block3 = io_block_set_read master_read (io_block_set_write master_write io_block2) in
        (* Build In/Outs for each instruction *)
        let complete_io_insts = inst_gen_io rw_io_insts in
            (* Place modified io_insts into block and return *)
            io_block_set_insts complete_io_insts in
        
    let initial_io_blocks = List.map build_io_block f in
raise Implement_Me

            



(*
  Build io_blocks for each block: func -> io_block list
      For each block:
      Get the read and writes for each element in the list: block -> io_inst list
      Build master read/write list: io_inst list -> ReadSet.t * WriteSet.t
      Build In/Out for each element. Start with last instruction in the list. io_inst list -> io_inst list (In/Out no longer empty)
      Build io_block: Read/Writes are master read writes. In/Out initialized to empty. insts to the io_insts list. mv_related = []; block to original block -> io_block list

  Loop until stable on io over blocks

  Build i_graph by running over blocks: io_block list -> interference graph
      For each block
          For each instruction:
          Add Ins to graph, if not there
          Mark all Ins as intfering with other Ins
          Add Outs to graph, if not there
          Mark all Outs as intfering with other Outs
      Mark all vars in block as interfering with intersection of block Ins with block Outs
          





  
*)

(*******************************************************************)
(* PS8 TODO:  graph-coloring, coalescing register assignment *)
(* You will need to build a mapping from variables to MIPS registers
   using the ideas behind the graph-coloring register allocation
   heuristics described in class.  This may involve spilling some
   of the variables into memory, so be sure to adjust the prelude
   of the function so that you allocate enough space on the stack
   to store any spilled variables.  The output should be a CFG
   function that doesn't use any variables (except for function
   names.)
*)
let reg_alloc (f : func) : func = 
    raise Implement_Me

(* Finally, translate the ouptut of reg_alloc to Mips instructions *)
let cfg_to_mips (f : func ) : Mips.inst list = 
    raise Implement_Me

