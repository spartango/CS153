(* This structure defines our control-flow graph intermediate representation,
 * and a compiler to map Cish code to cfg basic blocks.  For PS 7, your job
 * is for each function, to construct an interference graph that says
 * whether two temps have conflicting live ranges.  For PS 8, your job is
 * to implement the coallescing register allocator as described in class
 * (and in the book) to assign temps to physical registers and then map
 * the resulting code down to MIPS.  
 *)
module C = Cish_ast
type var = string
type label = string

exception Implement_Me

(* operands include scalars (ints), temps (variables), function names
 * (labels), and physical registers.  *)
type operand = Int of int | Var of var | Lab of label | Reg of Mips.reg
let sp = Reg(Mips.R29)
let fp = Reg(Mips.R30)
let ra = Reg(Mips.R31)

type compareop = Eq | Neq | Lt | Lte | Gt | Gte
type arithop = Plus | Minus | Times | Div 

(* essentially, a subset of the MIPS instructions but with
 * support for using temps as operands. *)
type inst = 
  Label of label
| Move of operand * operand                     (* x := y *)
| Arith of operand * operand * arithop * operand (* x := y + z *)
| Load of operand * operand * int               (* x := *(y+i) *)
| Store of operand * int * operand              (* *(x+i) := y *)
| Call of operand                               (* invoke f--result in R2 *)
| Jump of label  (* j L *)
| If of operand * compareop * operand * label * label
      (* if x < y then goto L1 else goto L2 *)
| Return  (* return to caller -- result assumed in R2 *)

(* basic blocks -- instead of capturing the structure of basic
 * blocks (as done in class), we just represent them as lists
 * of instructions where we assume that each block starts with a 
 * Label and ends with either a Jump, If, or Return and that 
 * there is no intervening Label, Jump, If, or Return. *)
type block = inst list 

(* a function is a list of basic blocks -- the first block is
 * assumed to be the entry point to the function. *)
type func = block list

(* a program is a list of functions -- the function named main
 * is considered the entry point. *)
type program = func list

let op2string = function
    (Int i) -> string_of_int i
  | (Var x) -> x
  | (Reg r) -> Mips.reg2string r
  | (Lab x) -> x

let arithop2string p = 
    match p with
      Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"

let compareop2string p = 
    match p with
        Eq -> "="
      | Neq -> "!="
      | Lt -> "<"
      | Gt -> ">"
      | Lte -> "<="
      | Gte -> ">="

let inst2string i = 
    match i with 
      (Label x) -> x^":"
    | Move(x,y) -> (op2string x) ^ " := " ^ (op2string y)
    | Arith(x,y,p,z) -> (op2string x) ^ " := " ^ (op2string y) ^
                        (arithop2string p) ^ (op2string z)
    | Load(x,y,i) -> (op2string x) ^ " := *(" ^ (op2string y) ^ "+" ^
                     (string_of_int i)^")"
    | Store(x,i,y) -> "*(" ^ (op2string x) ^ "+" ^ (string_of_int i) ^
                      ") := " ^ (op2string y)
    | Call x -> "call "^(op2string x)
    | Jump x -> "jump "^x
    | If(x,b,y,t,f) -> "if "^(op2string x)^(compareop2string b)^
                       (op2string y)^" goto "^t^" else goto "^f
    | Return -> "return"

let block2string b = 
    String.concat "" (List.map (fun i -> (inst2string i)^"\n") b)

let fun2string bs = 
    String.concat "" (List.map block2string bs)

let prog2string p = 
    String.concat "" (List.map fun2string p)

let inc r = let c = !r in r := c+1; c 
(* generate fresh labels *)
let label_counter = ref 0
let new_label() = ".L" ^ (string_of_int(inc label_counter))

(* generate fresh temps *)
let temp_counter = ref 0
let new_temp() = "x" ^ (string_of_int(inc temp_counter))

(*******************************************************************)
(* Translate a Cish function to a set of basic blocks.  Note
 * that this has *not* been heavily debugged so beware.  (I was
 * going to make you guys do this but then felt guilty.)  Nevertheless,
 * you should watch out for any bugs that are in the resulting code.
 *)
exception Impossible
    
let fn2blocks (C.Fn {C.name=name;C.args=args;C.body=body;C.pos=pos}) : block list = 
    let epilogue = new_label() in
    (* just keep a list of instructions around *)
    let curr_insts : inst list ref = ref [] in
    let emit_inst (i:inst):unit = curr_insts := i::(!curr_insts) in
    let empty_env (x:var) = Lab x in
    let extend env (x:var) operand = fun y -> if y = x then operand else env y in
    (* emits instructions to compute the expression's value
     * and returns the operand that holds that value. *)
    let rec emit_exp env (e,pos) : operand = ( 
        match e with
          C.Int i -> Int i
        | C.Var x -> env x
        (* arithmetic expressions *)
        | C.Binop(e1,((C.Plus | C.Minus | C.Times | C.Div) as p),e2) -> 
          emit_arith env e1 p e2
        (* conditional expressions -- compile as if we had
         * if (e) x=1 else x=0 *)
        | (C.Binop(_) | C.Not(_) | C.And(_) | C.Or(_)) -> 
            let x = new_temp() in
            let env = extend env x (Var x) in
            let s = (C.If((e,pos),
                    (C.Exp(C.Assign(x,(C.Int 1,pos)),pos),pos),
                    (C.Exp(C.Assign(x,(C.Int 0,pos)),pos),pos)),pos) in
            let _ = emit_stmt env s in
            Var x
        | C.Assign(x,e) -> 
            let y = emit_exp env e in
            let _ = emit_inst (Move(env x,y)) in 
            y
        | C.Load e -> 
            let t = Var(new_temp()) in
            let _ = emit_inst (Load(t,emit_exp env e,0)) in
            t
        | C.Store(e1,e2) -> 
            let x = emit_exp env e1 in
            let y = emit_exp env e2 in
            let _ = emit_inst (Store(x,0,y)) in 
            y
        | C.Malloc(e) -> 
            emit_exp env (C.Call((C.Var "malloc",pos),[e]),pos)
        | C.Call(e,es) -> 
            let t = Var(new_temp()) in
            let x = emit_exp env e in
            let xs = List.map (emit_exp env) es in
            let nargs = List.length xs in
            let arg_space = 4 * (if nargs <= 4 then 4 else nargs) in
            (* move arguments into appropriate positions *)
            let rec move_args = function
                (x::xs,0) -> 
                  (emit_inst (Move(Reg(Mips.R4),x)); move_args(xs,1))
              | (x::xs,1) ->
                  (emit_inst (Move(Reg(Mips.R5),x)); move_args(xs,2))
              | (x::xs,2) ->
                  (emit_inst (Move(Reg(Mips.R6),x)); move_args(xs,3))
              | (x::xs,3) -> 
                (emit_inst (Move(Reg(Mips.R7),x)); move_args(xs,4))
              | (x::xs,i) -> 
                  (emit_inst (Store(sp,4*i,x)); move_args(xs,i+1))
              | ([],_) -> () in
            (* allocate space for arguments *)
            let _ = emit_inst (Arith(sp,sp,Minus,Int(arg_space))) in
            (* move args to registers and stack *)
            let _ = move_args (xs,0) in
            (* call the function *)
            let _ = emit_inst (Call x) in
            (* deallocate space for args *)
            let _ = emit_inst (Arith(sp,sp,Plus,Int(arg_space))) in
            (* move result into temp t *)
            let _ = emit_inst (Move(t,Reg(Mips.R2))) in
            t
    )        
    (* specialized function for arithmetic binops *)
    and emit_arith env (e1:C.exp) (b:C.binop) (e2:C.exp) : operand = ( 
        let x = emit_exp env e1 in
        let y = emit_exp env e2 in
        let t = Var(new_temp()) in (* result temp *)
          (match b with
             C.Plus -> emit_inst(Arith(t,x,Plus,y))
           | C.Minus -> emit_inst(Arith(t,x,Minus,y))
           | C.Times -> emit_inst(Arith(t,x,Times,y))
           | C.Div -> emit_inst(Arith(t,x,Div,y))
           | _ -> raise Impossible); t        
    )
    (* specialize translation of conditional expressions in 
     * a testing context to jump to true_lab or false_lab. *)
    and emit_cond env ((r,pos) as e) (true_lab:label) (false_lab:label) = (
        match r with
          C.Not e -> emit_cond env e false_lab true_lab
        | C.And(e1,e2) -> 
            let t = new_label() in
            let _ = emit_cond env e1 t false_lab in
            let _ = emit_inst (Label t) in
            emit_cond env e2 true_lab false_lab
        | C.Or(e1,e2) -> 
            let f = new_label() in
            let _ = emit_cond env e1 true_lab f in
            let _ = emit_inst (Label f) in
            emit_cond env e2 true_lab false_lab
        | C.Binop(e1,C.Eq,e2) ->
            emit_inst (If(emit_exp env e1,Eq,emit_exp env e2,
                          true_lab,false_lab))
        | C.Binop(e1,C.Neq,e2) ->
            emit_inst (If(emit_exp env e1,Neq,emit_exp env e2,
                          true_lab,false_lab))
        | C.Binop(e1,C.Lt,e2) ->
            emit_inst (If(emit_exp env e1,Lt,emit_exp env e2,
                          true_lab,false_lab))
        | C.Binop(e1,C.Gt,e2) ->
            emit_inst (If(emit_exp env e1,Gt,emit_exp env e2,
                          true_lab,false_lab))
        | C.Binop(e1,C.Lte,e2) ->
            emit_inst (If(emit_exp env e1,Lte,emit_exp env e2,
                          true_lab,false_lab))
        | C.Binop(e1,C.Gte,e2) ->
            emit_inst (If(emit_exp env e1,Gte,emit_exp env e2,
                          true_lab,false_lab))
        | _ -> emit_inst(If(emit_exp env e,Neq,Int 0,true_lab,
                              false_lab))
    )             
    (* emit code for statements *)
    and emit_stmt env (s,pos) : unit = ( 
        match s with
          C.Exp e -> (let _ = emit_exp env e in ())
        | C.Seq(s1,s2) -> (let _ = emit_stmt env s1 in emit_stmt env s2)
        | C.If(e,s1,s2) -> 
            let (t,f,j) = (new_label(),new_label(),new_label()) in
            let _ = emit_cond env e t f in
            let _ = emit_inst (Label t) in
            let _ = emit_stmt env s1 in
            let _ = emit_inst (Jump j) in
            let _ = emit_inst (Label f) in
            let _ = emit_stmt env s2 in
            let _ = emit_inst (Jump j) in
            emit_inst (Label j)
        | C.While(e,s) ->
            let (test,loop,dOne) = (new_label(),new_label(),new_label()) in
            let _ = emit_inst (Jump test) in
            let _ = emit_inst (Label loop) in
            let _ = emit_stmt env s in
            let _ = emit_inst (Jump test) in
            let _ = emit_inst (Label test) in
            let _ = emit_cond env e loop dOne in
            emit_inst (Label dOne)
        | C.For(e1,e2,e3,s) -> 
            let _ = emit_exp env e1 in
            emit_stmt env (C.While(e2,(C.Seq(s,(C.Exp e3,pos)),pos)),pos)
        | C.Return e -> 
            let v = emit_exp env e in
            (* move result into R2 and jump to epilogue *)
            let _ = emit_inst (Move(Reg Mips.R2,v)) in
            let _ = emit_inst (Jump epilogue) in
            (* must insert another dummy label to keep basic blocks
             * well-formed *)
            emit_inst (Label (new_label()))
        | C.Let(x,e,s) -> 
            (* due to variable shadowing, we map each source-level
             * variable to a fresh temp using an environment. 
             *)
            let v = emit_exp env e in
            let x' = Var(new_temp()) in
            let env' = extend env x x' in
            let _ = emit_inst (Move(x',v)) in
            emit_stmt env' s
    ) in
    
    let callee_regs = 
        [fp;ra] @ (List.map (fun x -> Reg x) 
                      [Mips.R16;Mips.R17;Mips.R18;Mips.R19;Mips.R20;
                       Mips.R21;Mips.R22;Mips.R23]) in

    (* generate code to move the callee-saves registers into fresh
     * temps, and then return an association list mapping each callee-
     * saves register to its associated temp. *)
    let save_callee_regs () : (operand * operand) list = 
        List.map (fun r -> 
                    let t = Var(new_temp()) in
                    let _ = emit_inst (Move(t,r)) in
                    (r,t)
                 ) callee_regs in

    (* generate code to restore the callee-saves registers from their
     * associated temps. *)
    let restore_callee_regs reg_n_temp_list = 
        List.iter (fun (r,t) -> emit_inst (Move(r,t))) reg_n_temp_list in

    (* generate code to load any arguments into temps and return an
     * updated environment mapping the source-level names to their
     * associated temps. *)
    let rec load_args env plst =
        match plst with
          ([],_) -> env
        | (x::xs,i) ->
            let t = Var(new_temp()) in
            let new_env = extend env x t in
            let _ = (match i with
                      0 -> emit_inst (Move(t,Reg Mips.R4))
                    | 1 -> emit_inst (Move(t,Reg Mips.R5))
                    | 2 -> emit_inst (Move(t,Reg Mips.R6))
                    | 3 -> emit_inst (Move(t,Reg Mips.R7))
                    | _ -> emit_inst (Load(t,sp,i*4))) in
            load_args new_env (xs,i+1) in

    (* break the list of instructions into basic blocks *)
    let get_blocks insts = 
        (* a block should start with a label *)
        let rec get_block = function
          (Label x::insts) ->
            let (b,insts) = get_mid_block(insts,[Label x]) in
            Some(b, insts)
        | [] -> None
        | (i::rest) ->
            (print_string "error! "; print_string (inst2string i); print_string "\n";
             raise Impossible)
        (* the rest of the block should be terminated by a 
         * Return, If, or Jump. *)
        and get_mid_block (insts,prev) : block * (inst list) = 
            match insts with
              ((Return | If _ | Jump _) as i)::insts -> 
                (List.rev(i::prev),insts)
            | (Label _)::_ -> raise Impossible
            | [] -> raise Impossible
            | (i::insts) -> get_mid_block (insts,i::prev) in
        let rec get_bs (insts :inst list) : block list = 
            match get_block insts with
              Some (b,insts) -> b::(get_bs insts)
            | None -> [] in
        get_bs insts in
    (* emit prologue -- later, we'll have to add code to adjust the
     * stack pointer to set aside space to save any spilled values *)
    let _ = emit_inst (Label name) in
        (* generate temps for all of the callee-saves registers *)
    let callee_temps = save_callee_regs() in
        (* load any arguments into temps *)
    let env = load_args empty_env (args,0) in
    (* generate the body of the function *)
    let _ = emit_stmt env body in
    (* put in a dummy jump to the epilogue in case the program 
     * returned already *)
    let _ = emit_inst (Jump epilogue) in
    (* generate epilogue *)
    let _ = emit_inst (Label epilogue) in
    (* restore callee-saves registers *)
    let _ = restore_callee_regs callee_temps in
    (* return from function *)
    let _ = emit_inst Return in
    (* finally, break instructions into basic blocks *)
    get_blocks (List.rev(!curr_insts))
(* end fn2block *)
