open Cfg_ast
open I_graph
open Io_types
open Cfg_gen
open Reg_allocation
exception Implement_Me
exception FatalError

(*******************************************************************)
(* PS7 TODO:  interference graph construction *)

(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)
let build_interfere_graph (f : func) : interfere_graph =
    (* See cfg_gen.ml for build_io_block *)    
    let initial_io_blocks = List.map build_io_block f in
    let io_set_built_blocks = block_gen_io initial_io_blocks in
    (* See i_graph.ml for implementation *)
        build_igraph io_set_built_blocks



(* This magic is used to glue the generated lexer and parser together.
 * Expect one command-line argument, a file to parse.
 * You do not need to understand this interaction with the system. *)
let parse_file () =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Cish_parse.program Cish_lex.lexer (Lexing.from_channel ch)

let create_blocks prog = 
    List.map fn2blocks prog

let _ =
    let prog = parse_file () in
    let blocks = create_blocks prog in
    let _ = print_endline (prog2string blocks) in
    let igraphs = List.map build_interfere_graph blocks in
        List.map (fun ig -> print_endline (igraph2str ig)) igraphs



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

(* Helpers *)

(* High level *)
(* Removes*)



