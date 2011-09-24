open Lcombinators.GenericParsing
open Lcombinators.CharParsing

type pos = int  

(* the datatype for tokens -- you will need to augment these *)
type rtoken = 
	(* Operators *)
	Plus | Minus | Times | Div | Eq | Neq | Lt | Lte | Gt | Gte 
	(* Types *)
	| Int of int | Var of string | Not | And | Or | Assign  
	(* Control statements *)
	| Seq | If | While | For | Return | Else 
	(* Parens *)
	| LParen | RParen | LCurly | RCurly 

and token = rtoken * pos

let get_token_position (target_token : token) : pos =
    (snd target_token)

let get_token_rtoken (target_token : token) : rtoken =
    (fst target_token)

exception ImplementMe

(* Parser for alpha_numeric characters *)
let alpha_num : (char, char) parser = alt (alpha, dig)

(* the tokenize function -- should convert a list of characters to a list of 
 * Fish tokens using the combinators. *)
let rec tokenize(cs:char list) : token list = 
    raise ImplementMe
