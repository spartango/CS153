module ML = Mlish_ast
module S = Scish_ast

exception ImplementMe
let rec compile_exp ((e,_):ML.exp) : S.exp = raise ImplementMe
