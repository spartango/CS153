open Pretty_print
open Test_framework
open Cfg_ast
open I_graph
open Io_types
open Cfg_gen
open Test_ioblocks


;;

let node1 = build_node "n1" ["n2"]
let node2 = build_node "n2" ["n1"; "n3"]
let node3 = build_node "n3" ["n2"];;

let build_interfere_graph (f : func) : interfere_graph =
    (* See cfg_gen.ml for build_io_block *)    
    let initial_io_blocks = List.map build_io_block f in
    let io_set_built_blocks = block_gen_io initial_io_blocks in
    (* let _ = List.map (fun b -> print_endline (ioblock2str true false b)) initial_io_blocks in *)
    (* See i_graph.ml for implementation *)
        build_igraph io_set_built_blocks


let _ = print_endline (igraph2str (build_interfere_graph [example0_block0; example0_block1; example0_block2;]))
