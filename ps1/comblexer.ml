open Lcombinators.GenericParsing
open Lcombinators.CharParsing

(* the datatype for tokens -- you will need to augment these *)
type token = 
(* Operators *)
Plus | Minus | Times | Div | Eq | Neq | Lt | Lte | Gt | Gte |
(* Types *)
  Int of int | Var of var | Not | And | Or | Assign |  
(* Control statements *)
| Seq | If | While | For | Return |
(* Parens *)
LParen | RParen | LCurly | RCurly |
(* End of file *)

exception ImplementMe

(* the tokenize function -- should convert a list of characters to a list of 
 * Fish tokens using the combinators. *)
let rec tokenize(cs:char list) : token list = 
    raise ImplementMe
