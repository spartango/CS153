(* Type information for monadic form *)
module S = Scish_ast

type var = string

(* operands -- pure and small *)
type operand = Var of var | Int of int

(* values -- pure, but possibly large *)
type value = 
  Op of operand
| PrimApp of S.primop * (operand list)
| Lambda of var * exp

(* expressions -- possibly impure, control flow *)
and exp = 
  Return of operand
| LetVal of var * value * exp
| LetCall of var * operand * operand * exp
| LetIf of var * operand * exp * exp * exp

(* convert a monadic expression to a string *)
let exp2string (e:exp) : string = 
    let o2s = function
      (Var x) -> x
    | (Int i) -> string_of_int i in
    let rec e2s (tab:string) (e:exp) = 
        match e with
          Return w -> tab ^ (o2s w) ^ "\n"
        | LetCall(x,w1,w2,e) -> 
            tab ^"let "^x^" = "^(o2s w1)^"("^(o2s w2)^")\n"^(e2s tab e)
        | LetIf(x,w,e1,e2,e) -> 
            tab^"let "^x^" = if "^(o2s w)^" then\n"^
            (e2s (tab^"           ") e1)^
                  tab^"         else\n"^
            (e2s (tab^"           ") e2)^
            (e2s tab e)
        | LetVal(x,Op w,e) -> "let "^x^" = "^(o2s w)^"\n"^(e2s tab e)
        | LetVal(x,PrimApp(p,ws),e) -> 
            tab^"let "^x^" = "^(S.primop2string p)^"("^
            (String.concat "," (List.map o2s ws))^")\n"^(e2s tab e)
        | LetVal(x,Lambda(y,e1),e2) -> 
            tab^"fun "^x^"("^y^") =\n"^(e2s (tab^"     ") e1)^(e2s tab e2) in
    e2s "" e
