(* Prepends reversed x onto accum. Order of parameters for 
 * readability of code *)
let rec revapp (accum: 'a list) (x: 'a list) : 'a list=
    match x with
        | []         -> accum
        | head::tail -> revapp (head::accum) tail

let rev x = revapp [] x

(* Infix operator for revapp *)
let (<@) a b = revapp a b

module IntMap    = Map.Make(struct type t = int    let compare = compare end)
module StringMap = Map.Make(struct type t = String let compare = String.compare end)
