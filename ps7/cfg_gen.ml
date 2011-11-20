open Io_types
open Cfg_ast

let get_block_label (b: block) : label =
    match (List.hd b) with
        | Label l -> l
        | _ -> raise InvalidCFGCode

let get_block_children (b: block) : BlockSet.t = 
    let rec get_last_inst l = 
        match l with
            | [] -> raise InvalidCFGCode
            | h::[] -> h
            | h::t -> get_last_inst t in
        match get_last_inst b with
            | If(o1, compop, o2, l1, l2) -> BlockSet.add l2 (BlockSet.singleton l1)
            | Jump(l)                    -> BlockSet.singleton l
            | Return                     -> BlockSet.empty
            | _                          -> raise InvalidCFGCode (* Block does not end in control flow statement *)      


let get_rw (i: inst) : io_inst =
    (* Builds a set from a list of operands *)
    let set_of_ops (os: operand list) : VarSet.t =
        List.fold_left (fun a e -> match e with 
                            | Var x -> VarSet.add x a
                            | _ -> a) VarSet.empty os in
    let get_move_related (o1: operand) (o2: operand) : move_related = 
        match (o1, o2) with
            | (Var(x1),Var(x2)) -> (x1, x2)
            | _ -> raise InvalidCFGCode in 
    (* Builds a new io_inst record with reads rs and writes ws *)
    let set_rw (rs: operand list) (ws: operand list) = 
        io_inst_set_read (set_of_ops rs) (io_inst_set_write (set_of_ops ws) (new_io_inst i)) in
    match i with
        | Move(dest, src) -> io_inst_set_move [(get_move_related dest src)]
              (set_rw [src] [dest])
        | Arith(dest, s1, arthop, s2) ->
              set_rw [s1; s2] [dest]
        | Load(dest, adr, i) ->
              set_rw [adr] [dest]
        | Store(adr, i, src) ->
              set_rw [adr; src] []
        | Call(func_var) ->
              set_rw [func_var] []
        | Jump(l) ->
              set_rw [] []
        | Label l ->
              set_rw [] []
        | If(src1, compop, src2, lab1, lab2) ->
              set_rw [src1; src2] []
        | Return ->
              set_rw [] []
              
    
(*
  Move o1 o2 -> Write o1; Read o2
  Arith o1 o2 Arithop o3 -> Write o1; Read o2, o3
  Load o1 o2 Int -> Write o1; Read o2
  Store o1 Int o2 Write -; Read o1 o2
  Call o1 -> Write -; Read o1
  Jump l -> Write -; Read -
  If o1 Compareop o2 l1 l2 -> Write -; Read o1, o2
  Return -> Write -; Read -
*)

let gen_in (out_set : OutSet.t) 
           (read    : ReadSet.t) 
           (write   : WriteSet.t) : InSet.t =
  let o_sub = (VarSet.inter (VarSet.diff out_set write) out_set) in
  (VarSet.union read o_sub)

let gen_out (child_in_sets : InSet.t list) : OutSet.t = 
  List.fold_left VarSet.union VarSet.empty child_in_sets
      
let inst_gen_in (target : io_inst) : io_inst =
  io_inst_set_in 
    (gen_in target.inst_out target.inst_read target.inst_write)
    target

let inst_gen_out (target : io_inst) (next : io_inst) : io_inst =
  io_inst_set_out
    (gen_out [next.inst_in;])
    target

let block_gen_in (target : io_block) : io_block =
  io_block_set_in 
    (gen_in target.block_out target.master_read target.master_write)
    target

let block_gen_out (blocks : io_block list) (target : io_block) : io_block =
  io_block_set_out
    (gen_out 
      (set_map (fun child_name -> 
                  let blk = (lookup_block child_name blocks) in
                  blk.block_in) 
                target.children) 
    )
    target

let inst_gen_io (target: io_inst list) : io_inst list =
    List.fold_left (fun accum io_i ->
                        (* next_ins holds state *)
                        let(io_inst_list, next_ins) = accum in
                        let new_io_i = inst_gen_in (inst_gen_out io_i next_ins) in
                            (new_io_i::io_inst_list, new_io_i.inst_in)) ([], InSet.empty) target

let block_gen_io (target: io_block list) : io_block list =
  run_until_stable 
    (fun () -> 
      List.map (block_gen_in block_gen_out) target
    )
    10
