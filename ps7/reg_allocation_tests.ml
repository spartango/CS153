open Test_framework
open Pretty_print
open Cfg_ast
open I_graph
open Cfg_gen
open Test_ioblocks
open Reg_allocation

let node1 = build_node "n1" ["n2";"n4"]
let node2 = build_node "n2" ["n1";"n3";"n4"]
let node3 = build_node "n3" ["n2";"n4"];;
let node4 = build_node "n4" ["n1";"n2";"n3"]

let test_graph1 = build_graph [node1; node2; node3; node4]

let mk_simplify_test_graph (g: interfere_graph) (regs: int) (exp: interfere_graph) (name: string) =
    mk_verbose_expect_test (fun () -> let (g, stack) = simplify regs g [] in g) exp igraph2str name

let build_interfere_graph (f : func) : interfere_graph =
    (* See cfg_gen.ml for build_io_block *)    
    let initial_io_blocks = List.map build_io_block f in
    let io_set_built_blocks = block_gen_io initial_io_blocks in
    (* let _ = List.map (fun b -> print_endline (ioblock2str true false b)) initial_io_blocks in *)
    (* See i_graph.ml for implementation *)
        build_igraph io_set_built_blocks


let simplify_test1 = mk_simplify_test_graph test_graph1 4 IGNodeSet.empty "Test completely reducable graph";;
let simplify_test2 = mk_simplify_test_graph (build_interfere_graph [example0_block0; example0_block1; example0_block2;])
    4 IGNodeSet.empty "Lecture example with 4 registers";;
let simplify_test3 = mk_simplify_test_graph (build_interfere_graph [example0_block0; example0_block1; example0_block2;])
    3 IGNodeSet.empty "Lecture example with 3 registers";;

run_test_set [simplify_test1; simplify_test2; simplify_test3] "Simplify tests";;
