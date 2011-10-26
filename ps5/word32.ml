(* Imitates SML/NJ's Word32 Module *)
open Int32

type word = int32

let fromInt n = Int32.of_int n

let toString w = Int32.to_string w

let toStringX w = 
  if w = 0x80000000l
  then "0x80000000"
  else Printf.sprintf "0x%lX" w

let andb (w1,w2) = Int32.logand w1 w2

let neg w = Int32.neg w

