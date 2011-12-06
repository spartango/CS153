open Test_framework
open Pretty_print
open Cfg_ast
open I_graph
open Reg_allocation

let node1 = build_node "n1" ["n2";"n4"]
let node2 = build_node "n2" ["n1"; "n3";"n4"]
let node3 = build_node "n3" ["n2";"n4"];;
let node4 = build_node "n4" ["n1";"n2";"n3"]

let test_graph1 = build_graph [node1; node2; node3; node4]

