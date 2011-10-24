open Cish_ast
open Mips

let (stub_pos : Cish_ast.pos)= 12;;
let null     = Int(0);;

let result_name = "result";;

exception InvalidExpressionListLength
let verify_length (ls: 'a list) (expected: int): unit =
    if (List.length ls) = expected
    then ()
    else raise InvalidExpressionListLength

(* Glues a list of statements together with Seq *)
let rec seqs (stmts : Cish_ast.stmt list) : Cish_ast.stmt =
    match stmts with
        | [] -> (Cish_ast.skip, stub_pos)
        | hd::[] -> hd
        | hd::tl -> (Cish_ast.Seq(hd, seqs tl), stub_pos)


(* Will only convert a single stmt concluded by a semicolon *)
let cish_stmt_from_str (s : string) : Cish_ast.stmt = 
  Cish_parse.stmt Cish_lex.lexer (Lexing.from_string s)

(* Wraps a let declaration for v around st *)
let init_var (v: string) (st: Cish_ast.stmt) : Cish_ast.stmt =
    (Cish_ast.Let(v, (null, stub_pos), st), stub_pos)
let return_result (code : stmt) : stmt =
    (Cish_ast.Seq(code, cish_stmt_from_str ("return " ^ result_name ^ ";")), stub_pos)
let init_result (code : stmt) : stmt =
    init_var "result" code

let zip_cish_strs (strs: string list) : Cish_ast.stmt =
    seqs (List.map cish_stmt_from_str strs)

(* Arguments to reversed list functor *)
module type LISTARGS =
    sig
        type element
    end

module type RLIST =
  sig
      type element
      type rlist
      val empty    : rlist
      val app_list : rlist -> element list -> rlist
      val rev_list : element list -> rlist
      val to_list  : rlist -> element list
  end

(* Reverse list module *) 
module RevList (L: LISTARGS) : (RLIST with
                                          type element = L.element)=  
    struct
        type element = L.element
        type rlist = (element list) 
        let empty = []
        let rec app_list (accum: rlist) (x: element list) : rlist=
            match x with
                | []         -> accum
                | head::tail -> app_list (head::accum) tail
        let rev_list (l: element list) : rlist = app_list [] l                      
        let to_list (x: rlist) : element list = app_list [] x      
    end

(* Reverse lists for MIPS instructions *)
module RInstList = RevList(struct type element = inst end)

(* Sugar for appending a list to an rlist *)
let (<@) (a: RInstList.rlist) (b: inst list) = RInstList.app_list a b

module IntMap    = Map.Make(struct type t = int      let compare = compare end)
module StringMap = Map.Make(struct type t = String.t let compare = String.compare end)


