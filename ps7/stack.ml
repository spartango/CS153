open Cfg_ast

module type STACK =
    sig 
        type t
        type elt
        val push : elt -> t -> t
        val pop : t -> (elt * t) option 
        val empty : t
    end

module type STACK_ARGS =
    sig
        type t
    end 

module SimpleStack (A: STACK_ARGS) : (STACK with type elt = A.t) =
    struct
        type elt = A.t
        type t = elt list
        let push (e: elt) (s: t) : t =
            e::s
        let pop (s: t) : (elt * t) option =
            match s with
                | [] -> None
                | hd::tl -> Some (hd, tl)
        let empty = []
    end 

type var_stack_element = Single of var | Coalesced of var list | Spill of var

module VarStack = SimpleStack(struct type t = var_stack_element end);;
