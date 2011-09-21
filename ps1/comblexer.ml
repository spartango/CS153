open Lcombinators.GenericParsing
open Lcombinators.CharParsing

(* the datatype for tokens -- you will need to augment these *)
type token = 
	(* Operators *)
	Plus | Minus | Times | Div | Eq | Neq | Lt | Lte | Gt | Gte |
	(* Types *)
	Int of int | Id of string | Not | And | Or | Assign   
	(* Control statements *)
	| Seq | If | While | For | Return |
	(* Parens *)
	LParen | RParen | LCurly | RCurly 
exception ImplementMe

let tkn2str (t: token) : string = 
    match t with 
        | Id(v) -> "Id(" ^ v ^ ")"
        | Int(i) -> string_of_int i
        | _ -> raise ImplementMe


(* Combinators for lexer *)

(* General parser for alpha_numeric characters *)
let alpha_num : (char, char) parser = alt (alpha, dig)

(* Lexer for variables *)
let id_combinator = map (fun v -> (Id v)) identifier

(* Lexer for integers *)
let int_combinator = map (fun v -> (Int v)) integer

(* the tokenize function -- should convert a list of characters to a list of 
 * Fish tokens using the combinators. *)
let rec tokenize(cs:char list) : token list = 
    raise ImplementMe
    (* Declare combinators *)
    (* Create massive single combinator with alts *)
    (* Run combinator over cs *)
        (* Return token list on end of cs *)
        (* Handle errors when combinator returns Nil *)
