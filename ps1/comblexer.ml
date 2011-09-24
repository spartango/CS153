open Lcombinators.GenericParsing
open Lcombinators.CharParsing
open Explode

(* the datatype for tokens -- you will need to augment these *)
type token = 
	(* Operators *)
	Plus | Minus | Times | Div | Eq | Neq | Lt | Lte | Gt | Gte |
	(* Types *)
	Int of int | Id of string | Not | And | Or | Assign |  
	(* Control statements *)
	Seq | If | Else | While | For | Return |
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
        | Not -> "!"
        | And -> "&&"
        | Or -> "||"
        | If -> "if"
        | Seq -> ";"
        | Neq -> "!="
        | Eq -> "=="
        | Div -> "/"
        | Times -> "*"
        | Else -> "else"
        | While -> "while"
        | For -> "for"
        | Return -> "return"
        | LParen -> "("
        | RParen -> ")"
        | LCurly -> "{"
        | RCurly -> "}"

(* Combinators for lexer *)

let c_combinator (ch: char) (t: token) =
    map (fun _ -> t) (c ch)

let str_combinator (st: string) (t: token) =
    map (fun _ -> t) (str st)

(*** Operators ***)
let plus_combinator = c_combinator '+' Plus
let minus_combinator = c_combinator '-' Minus
let times_combinator = c_combinator '*' Times
let div_combinator = c_combinator '/' Div
let eq_combinator = str_combinator "==" Eq
let neq_combinator = str_combinator "!=" Neq
let gte_combinator = str_combinator ">=" Gte
let gt_combinator = c_combinator '>' Gt
let lte_combinator = str_combinator "<=" Lte
let lt_combinator = c_combinator '<' Lt

(* Types *)
(* Combinator for integers *)
let int_combinator = map (fun v -> (Int v)) integer
(* Combinator for variables *)
let id_combinator = map (fun v -> (Id v)) identifier
(* Not Combinator *)
let not_combinator = c_combinator '!' Not
(* And Combinator *)
let and_combinator = str_combinator "&&" And
(* Or Combinator *)
let or_combinator = str_combinator "||" Or
(* Assignment *)
let assign_combinator = c_combinator '=' Assign

(* Control Statements *)
let seq_combinator = c_combinator ';' Seq
let for_combinator = str_combinator "for" For
let if_combinator = str_combinator "if" If
let else_combinator = str_combinator "else" Else
let while_combinator = str_combinator "while" While
let return_combinator = str_combinator "return" Return

(* Parentheses *)
let lparen_combinator = c_combinator '(' LParen
let rparen_combinator = c_combinator ')' RParen
let lcurly_combinator = c_combinator '{' LCurly
let rcurly_combinator = c_combinator '}' RCurly

(* Complete combinator *)
let complete_combinator = 
    map (fun r -> let(_,t) = r in t)
        (seq(whitespace,
            (alts [
                 (* Keyword combinators must come before id *)
                 for_combinator;
                 if_combinator;
                 else_combinator;
                 while_combinator;
                 return_combinator;
                 id_combinator; 
                 int_combinator; 
                 seq_combinator;
                 plus_combinator;
                 minus_combinator;
                 times_combinator;
                 div_combinator;
                 (* Neq (!=) must come before Not (!) *)
                 neq_combinator;
                 not_combinator;
                 (* Gte (>=) must come before Gt (>) *)
                 gte_combinator;
                 gt_combinator;
                 (* Lte (<=) must come before Lt (<) *)
                 lte_combinator;
                 lt_combinator;
                 eq_combinator;
                 and_combinator;
                 or_combinator;
                 lparen_combinator;
                 rparen_combinator;
                 lcurly_combinator;
                 rcurly_combinator;
                 assign_combinator;])))

(* the tokenize function -- should convert a list of characters to a list of 
 * Fish tokens using the combinators. *)
let rec tokenize(cs:char list) : token list = 
    match cs with
        | [] -> []
        | _ ->
              match complete_combinator cs with
                  | Cons((tkn, cs_tail), _) -> 
                            tkn::(tokenize cs_tail)
                  | Nil ->
                        let _ = print_string (implode cs) in 
                        raise LexError
