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

let is_simplified (g: interfere_graph) (regs: int) : bool =
    IGNodeSet.fold (fun e accum -> (not (is_simplifiable e regs)) && accum) g true

let mk_simplify_test_graph (g: interfere_graph) (regs: int) (exp: interfere_graph) (name: string) =
    mk_generic_equals_test IGNodeSet.equal (fun () -> let (g, stack) = simplify regs g in g) exp igraph2str name

let build_interfere_graph (f : func) : interfere_graph =
    (* See cfg_gen.ml for build_io_block *)    
    let initial_io_blocks = List.map build_io_block f in
    let io_set_built_blocks = block_gen_io initial_io_blocks in
    (* let _ = List.map (fun b -> print_endline (ioblock2str true false b)) initial_io_blocks in *)
    (* See i_graph.ml for implementation *)
        build_igraph io_set_built_blocks

let example0_igraph = (build_interfere_graph [example0_block0; example0_block1; example0_block2;])
let example0_expected_4regs =
    let b = ignode_set_moves (IGMoveSet.singleton {node_var = "b"; interfere_var = "j"}) (build_node "b" ["d";"c"]) in
    let c = ignode_set_moves (IGMoveSet.singleton {node_var = "c"; interfere_var = "d"}) (build_node "c" ["b"]) in
    let d = ignode_set_moves (IGMoveSet.singleton {node_var = "d"; interfere_var = "c"}) (build_node "d" ["j";"b"]) in
    let j = ignode_set_moves (IGMoveSet.singleton {node_var = "j"; interfere_var = "b"}) (build_node "j" ["d"]) in
        build_graph [b;c;d;j] 
let example0_expected_3regs =
    let b = ignode_set_moves (IGMoveSet.singleton {node_var = "b"; interfere_var = "j"}) (build_node "b" ["d";"c"]) in
    let c = ignode_set_moves (IGMoveSet.singleton {node_var = "c"; interfere_var = "d"}) (build_node "c" ["b"]) in
    let d = ignode_set_moves (IGMoveSet.singleton {node_var = "d"; interfere_var = "c"}) (build_node "d" ["j";"b"]) in
    let j = ignode_set_moves (IGMoveSet.singleton {node_var = "j"; interfere_var = "b"}) (build_node "j" ["d"]) in
        build_graph [b;c;d;j] 


let simplify_test1 = mk_simplify_test_graph test_graph1 4 IGNodeSet.empty "Test completely reducable graph";;
let simplify_test1a = mk_verbose_expect_test (fun () -> let (g, v_stack) = (simplify 4 test_graph1) in
                                                  is_simplified g 4) true string_of_bool "Test completely reducable graph";;
let simplify_test2 = mk_simplify_test_graph example0_igraph 4 example0_expected_4regs "Lecture example with 4 registers";;
let simplify_test3 = mk_verbose_expect_test (fun () -> let (g, v_stack) = (simplify 3 example0_igraph) in
                                             let _ = print_endline (igraph2str g) in
                                                  is_simplified g 3) true string_of_bool "Lecture example with 3 registers for reduction";;

run_test_set [simplify_test1; simplify_test2; simplify_test3] "Simplify tests";;
