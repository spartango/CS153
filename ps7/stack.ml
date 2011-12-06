open Cfg_ast

module VarStack = 
    struct
        type t = var list
        let push (e: var) (s: t) : t =
            e::s
        let pop (s: t) : (var * t) option =
            match s with
                | [] -> None
                | hd::tl -> Some (hd, tl)
    end 
