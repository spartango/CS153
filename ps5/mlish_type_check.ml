open Mlish_ast

exception TypeError
let type_error(s:string) = (print_string s; raise TypeError)

let type_check_exp (e:Mlish_ast.exp) : tipe = raise TypeError
