open Cfg_ast
open I_graph

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

type var_stack_element = Single of var | Coalesced of var list | Spill of var | Spill_Coalesced of var list

module VarStack = SimpleStack(struct type t = var_stack_element end);;

(* Push a node onto a var stack, check to see if that node is coalesced *)
let push_spill_node (node: ignode) (v_stack: VarStack.t) : VarStack.t =
     match node.coalesced with
        | None -> VarStack.push (Spill(node.name)) v_stack
        | Some coalesced_vars -> VarStack.push (Spill_Coalesced(node.name::coalesced_vars)) v_stack

(* Push a node onto a var stack, check to see if that node is coalesced *)
let push_node (node: ignode) (v_stack: VarStack.t) : VarStack.t =
    match node.coalesced with
        | None ->
              let _ = print_endline node.name in
 VarStack.push (Single(node.name)) v_stack
        | Some coalesced_vars -> VarStack.push (Coalesced(node.name::coalesced_vars)) v_stack

let pop_var_stack v_stack =
    VarStack.pop v_stack
