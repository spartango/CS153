(* Contains definitions for operations on 32-bit words.
 * Uses Ocamls' Int32 module. *)
open Int32

type word = int32

(* Convert an int into a word. You will want to use this function for
 * Mips operations that require an immediate. *)
let fromInt n = Int32.of_int n

(* You can ignore the functions below. They are just used in the Mips simulator *)
let toString w = Int32.to_string w

let toStringX w = 
  if w = 0x80000000l
  then "0x80000000"
  else Printf.sprintf "0x%lX" w

let andb (w1,w2) = Int32.logand w1 w2

let neg w = Int32.neg w

