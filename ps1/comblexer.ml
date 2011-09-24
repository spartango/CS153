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
        | Gte -> ">="
        | Gt -> ">"
        | Lte -> "<="
        | Lt -> "<"
        | _ -> ""

(* Combinators for lexer *)

let c_combinator (ch: char) (t: token) =
    map (fun _ -> t) (c ch)

let str_combinator (st: string) (t: token) =
    map (fun _ -> t) (str st)

(* Operators *)
let plus_combinator = c_combinator '+' Plus

let minus_combinator = c_combinator '-' Minus

let times_combinator = c_combinator '*' Times

let div_combinator = c_combinator '/' Div

let neq_combinator = str_combinator "!=" Neq

let gte_combinator = str_combinator ">=" Gte

let gt_combinator = c_combinator '>' Gt

let lte_combinator = str_combinator "<=" Lte

let lt_combinator = c_combinator '<' Lt

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
                  div_combinator;
                  neq_combinator;
                  gte_combinator;
                  gt_combinator;
                  lte_combinator;
                  lt_combinator;
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
