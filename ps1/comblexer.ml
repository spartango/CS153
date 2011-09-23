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
exception LexError

let tkn2str (t: token) : string = 
    match t with 
        | Id(v) -> "Id(" ^ v ^ ")"
        | Int(i) -> string_of_int i
        | Plus -> "+"
        | Minus -> "-"
        | Assign -> "="
        | _ -> raise ImplementMe

(* Combinators for lexer *)

let c_combinator (ch: char) (t: token) =
    map (fun _ -> t) (c ch)

(* Operators *)
let plus_combinator = c_combinator '+' Plus

let minus_combinator = c_combinator '-' Minus

let times_combinator = c_combinator '*' Times

(* Assignment *)
let assign_combinator = c_combinator '=' Assign

(* Combinator for variables *)
let id_combinator = map (fun v -> (Id v)) identifier

(* Combinator for integers *)
let int_combinator = map (fun v -> (Int v)) integer

(* Complete combinator *)
let complete_combinator = 
    map (fun r -> let(_,t) = r in t)
        (seq(whitespace,
            (alts [id_combinator; 
                  int_combinator; 
                  plus_combinator;
                  minus_combinator;
                  times_combinator;
                  assign_combinator])))

(* the tokenize function -- should convert a list of characters to a list of 
 * Fish tokens using the combinators. *)
let rec tokenize(cs:char list) : token list = 
    match cs with
        | [] -> []
        | _ ->
              match complete_combinator cs with
                  | Cons((tkn, cs_tail), _) -> 
                            tkn::(tokenize cs_tail)
                  | Nil -> raise LexError
