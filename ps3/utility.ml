(* Prepends reversed x onto accum. Order of parameters for 
 * readability of code *)
let rec revapp (accum: 'a list) (x: 'a list) : 'a list=
    match x with
        | []         -> accum
        | head::tail -> revapp (head::accum) tail

let rev x = revapp [] x