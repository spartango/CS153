open Lcombinators.GenericParsing
open Lcombinators.CharParsing

(* the datatype for tokens -- you will need to augment these *)
type token = 
	(* Operators *)
	Plus | Minus | Times | Div | Eq | Neq | Lt | Lte | Gt | Gte |
	(* Types *)
	Int of int | Var of string | Not | And | Or | Assign   
	(* Control statements *)
	| Seq | If | While | For | Return |
	(* Parens *)
	LParen | RParen | LCurly | RCurly 

exception ImplementMe

(* Parser for alpha_numeric characters *)
let alpha_num : (char, char) parser = alt (alpha, dig)

(* Parser for variable *)
let var = seq(lc_alpha, plus(alt(alpha_num, underscore)))

(* the tokenize function -- should convert a list of characters to a list of 
 * Fish tokens using the combinators. *)
let rec tokenize(cs:char list) : token list = 
    raise ImplementMe
    (* Declare combinators *)
    (* Create massive single combinator with alts *)
    (* Run combinator over cs *)
        (* Return token list on end of cs *)
        (* Handle errors when combinator returns Nil *)
