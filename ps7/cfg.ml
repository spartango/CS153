open Cfg_ast
open I_graph
open Io_types
open Cfg_gen
open Reg_allocation
open Cfg_to_mips
exception Implement_Me
exception FatalError

(*******************************************************************)
(* PS7 TODO:  interference graph construction *)

(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)

let build_interfere_graph (f: func) : interfere_graph =
    I_graph.build_interfere_graph f


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
let regs = ref 24

let reg_alloc (f : func) : func = 
    let igraph = build_interfere_graph f in
    let reduced_state = mark_spill 
        (freeze (coalesce (simplify 
                               (initial_reduction_state igraph 24 f)))) in
    let colored_state = propogate_precolored (color_graph reduced_state) in
    let colored_code = rewrite_code colored_state in
        colored_code                            

(* Finally, translate the ouptut of reg_alloc to Mips instructions *)
let cfg_to_mips (f : func ) : Mips.inst list = 
    let _ = print_endline (fun2string f) in
    List.fold_right 
        (fun b accumulated_mips -> (block_to_mips b) @ accumulated_mips)
        f
        []


(* Executable will take file and return register allocated mips *)

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
    (* Cish program - list of Cish functions *)
    let prog = parse_file () in
    let reg_allocated_functions = 
        List.map (fun cish_func -> reg_alloc (fn2blocks cish_func)) prog in
    let mips_functions = List.map cfg_to_mips reg_allocated_functions in
    let mips_prog = List.flatten mips_functions in
        print_endline (Mips.mips2str mips_prog)



