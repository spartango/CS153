open Mips

let (stub_pos : Cish_ast.pos)= 12;;

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


let cish_stmt_from_str (s : string) : Cish_ast.stmt = 
    let _ = print_string ("Compile stmt: "^ s ^ "\n") in
  Cish_parse.stmt Cish_lex.lexer (Lexing.from_string s)

