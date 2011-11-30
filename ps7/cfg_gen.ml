open Io_types
open Cfg_ast
open Utility 

exception InvalidLabel
exception FailedStabilization
exception InvalidControlFlow
exception InvalidMoveRelated

let run_until_stable t_func init_arg limit =
  let rec until_stable arg count =
    if count >= limit then raise FailedStabilization
    else
      let out = t_func arg in
      if (io_block_list_equal out arg) then let _ = print_string ("Ran "^(string_of_int count)^"x\n") in out
      else until_stable out (count + 1) 
  in
  until_stable init_arg 1

let get_block_label (b: block) : label =
    match (List.hd b) with
        | Label l -> l
        | _ -> raise InvalidLabel

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
            | _                          -> raise InvalidControlFlow (* Block does not end in control flow statement *)      


let get_rw (i: inst) : io_inst =
    (* Builds a set from a list of operands *)
    let set_of_ops (os: operand list) : VarSet.t =
        List.fold_left (fun a e -> match e with 
                            | Var x -> VarSet.add x a
                            | _ -> a) VarSet.empty os in
    let get_move_related (o1: operand) (o2: operand) : move_related = 
        match (o1, o2) with
            | (Var(x1),Var(x2)) -> (x1, x2)
            | _ -> raise InvalidMoveRelated in 
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
  let r_sub = (VarSet.inter (VarSet.diff read write) read) in
  (VarSet.union r_sub o_sub)

let gen_out (child_in_sets : InSet.t list) : OutSet.t = 
  List.fold_left VarSet.union VarSet.empty child_in_sets
      
let inst_gen_in (target : io_inst) : io_inst =
  io_inst_set_in 
    (gen_in target.inst_out target.inst_read target.inst_write)
    target

let inst_gen_out (target : io_inst) (next_ins : InSet.t) : io_inst =
  io_inst_set_out
    (gen_out [next_ins;])
    target

let block_gen_in (target : io_block) : io_block =
  io_block_set_in 
    (gen_in target.block_out target.master_read target.master_write)
    target

let block_gen_out (blocks : io_block list) (target : io_block) : io_block =
  io_block_set_out
    (gen_out 
      (set_map (fun (child_name : label)  -> 
                  let blk = (lookup_block child_name blocks) in
                  blk.block_in) 
                target.children) 
    )
    target

(* Builds the In/Out sets for each instruction. block_out is the Out set for the final instruction *)
let inst_gen_io_base (block_out: OutSet.t) (target: io_inst list) : io_inst list =
    let target1 = List.rev target in
    let (modified, _) = List.fold_left (fun accum io_i ->
                        (* next_ins holds state *)
                                            let (io_inst_list, next_ins) = accum in
                                            let new_io_i = inst_gen_in (inst_gen_out io_i next_ins) in
                                                (new_io_i::io_inst_list, new_io_i.inst_in)) ([], block_out) target1
    in modified

(* Builds In/Out sets for each instruction where the Out set for the last instruction is empty *)
let inst_gen_io (target: io_inst list) : io_inst list =
    inst_gen_io_base OutSet.empty target


(* Fold right version of this - it's clearly the better version, but Anand doesn't like it *)
(*
    let (modified, _) = List.fold_right (fun io_i accum ->
                        (* next_ins holds state *)
                                            let (io_inst_list, next_ins) = accum in
                                            let new_io_i = inst_gen_in (inst_gen_out io_i next_ins) in
                                                (new_io_i::io_inst_list, new_io_i.inst_in)) target ([], InSet.empty)
    in modified
*)

let block_gen_io (io_blks: io_block list) : io_block list =
    let step target = List.map (fun t -> (block_gen_in (block_gen_out target t))) in
    let s1 = step io_blks io_blks in
    let s2 = step s1 s1 in
    let s3 = step s2 s2 in
    let s4 = step s3 s3 in
    s4

let build_io_block (b: block) : io_block =
    (* Generate empty io_block - leave ins, outs, and moves empty *)
    let io_block0 = new_io_block b in
    (* Get block's label *)
    let io_block1 = io_block_set_label (get_block_label b) io_block0 in
    (* Get block's children *)
    let io_block2 = io_block_set_children (get_block_children b) io_block1 in
    (* Build io_insts for block's instrucitons *)
    let rw_io_insts = List.map get_rw b in
    (* Build master read/write sets for block *)
    let (master_read, master_write, master_moves) = 
        List.fold_left 
            (fun accumulated io_rec ->
                 let(reads, writes, moves) = accumulated in
                     (ReadSet.union io_rec.inst_read reads,
                      WriteSet.union io_rec.inst_write writes, 
                      moves @ io_rec.inst_move)
            ) 
            (ReadSet.empty, WriteSet.empty, []) 
            rw_io_insts 
    in
        (* Add master read/writes to io_block *)
    let io_block3 = io_block_set_move master_moves 
        (io_block_set_read master_read 
             (io_block_set_write master_write io_block2)) in
        (* Build In/Outs for each instruction *)
    let complete_io_insts = inst_gen_io rw_io_insts in
        (* Place modified io_insts into block and return *)
        io_block_set_insts complete_io_insts io_block3
            
