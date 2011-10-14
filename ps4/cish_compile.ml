exception FAILBLOG
(* Compile Cish AST to MIPS AST *)
open Mips
open Cish_ast
open Utility
open Word32

exception TODO

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

(* Stack Manipulation *)

(* Offset is with respect to the Frame Pointer (fp) *)
type virtualStack = {  last_offset : int32;
                       contents    : int32 StringMap.t}

(* Code Gen *)

let sanitize_f_name (f : string) : string=
  if (f <> "main") 
    then "f_"^f 
    else f

(* Generates code to push a variable on to the stack *)
let add_local_var (v : string) (stack : virtualStack) : virtualStack * inst list =
    (* Push variable on to stack *)
    (* Variable is an aligned 32 bit int *)
    let new_contents = StringMap.add v stack.last_offset stack.contents in
    let new_stack = { last_offset = (Int32.add stack.last_offset (-4l)) ; contents = new_contents } in
    (* Generate corresponding instructions *)
    (* Move $sp *)
    let insts = [ Add(sp, sp, Immed(-4l)); ] in
    (new_stack, insts)

(* Generates code to pop a variable off the stack *)
let pop_local_var (v : string) (stack : virtualStack) : virtualStack * inst list =
    let new_contents = StringMap.remove v stack.contents in
    let new_stack = { last_offset = (Int32.add stack.last_offset 4l) ; contents = new_contents } in
    let insts = [ Add(sp, sp, Immed(4l)); ] in
    (new_stack, insts)

(* Provides the offset of a variable relative to the stack ptr *)
let find_local_var (v : string) (stack : virtualStack) : int32 = 
    StringMap.find v stack.contents

(* Generates code to create a new temporary var *)
let rec new_temp (stack : virtualStack) : string * virtualStack * inst list = 
    (* Create a variable, add it *)
    let name = "T"^ (string_of_int (new_int ())) in
    let (new_stack, insts) = add_local_var name stack in
    (name, new_stack, insts)


(* Function prologue generation *)
let generate_prologue (f_sig : funcsig) (stack : virtualStack) : virtualStack * inst list =
    let n_args = List.length f_sig.args in
    let arg_offset = (Int32.mul (-4l) (Int32.of_int n_args)) in

    (* Save the old FP and set new FP *)
    let insts = [ Sw (fp, sp, arg_offset); 
                  Add(fp, sp, Reg(R0)); ]
    in

    let rec mark_high_args skipped_num arg_names t_stack t_insts =
        if skipped_num < 4 
        then mark_high_args (skipped_num + 1) (List.tl arg_names) t_stack t_insts
        else
            match arg_names with
            | []       -> (t_stack, t_insts)
            | hd::rest -> let (new_stack, new_insts) = add_local_var hd t_stack in
                          mark_high_args skipped_num rest new_stack (t_insts @ new_insts)
    in 
    
    let rec save_low_args touched_num arg_names t_stack t_insts =
        if touched_num >= 4 
        then (t_stack, t_insts)
        else
            match arg_names with
            | []       -> (t_stack, t_insts)
            | hd::rest -> let (new_stack, new_insts) = add_local_var hd t_stack in
                          let new_insts = new_insts @ 
                                          [ Sw((string2reg ("A"^(string_of_int touched_num))), 
                                                fp, 
                                                (find_local_var hd new_stack)); ] 
                          in
                          save_low_args (touched_num + 1) rest new_stack (t_insts @ new_insts)
    in 

    (* Then the first 4 args *)
    (* Save the rest of the arguments a0 - a3 *)
    let (new_stack, narg_insts) = save_low_args 0 f_sig.args stack []
    in

    (* Mark the stack positions of arguments *)
    (* First are n_arg > 3 *)
    let (new_stack, arg_insts) = if (arg_offset > -16l) 
                                 then (new_stack, []) 
                                 else mark_high_args 0 f_sig.args new_stack []
    in

    (* Mark the Framepointer *)
    let (new_stack, fp_insts) =  add_local_var "FP" new_stack in


    (* Save Callee saved registers: $ra, and $s0-$s7 ($16-$23) *)
    let (new_stack, ra_insts) =  add_local_var "RA" new_stack in
    let ra_insts = ra_insts @ [ Sw(ra, fp, (find_local_var "RA" new_stack)); ] in

    let rec save_sregs (num : int) (t_stack : virtualStack) (t_insts : inst list) =
        if num < 0 
        then (t_stack, t_insts) 
        else 
            let name = "S"^(string_of_int num) in
            let (new_stack, s_insts) = add_local_var name t_stack in
            let new_insts = s_insts @ [ Sw((string2reg name), fp, (find_local_var name new_stack)); ] in
            save_sregs (num - 1) new_stack (t_insts @ new_insts)
    in 

    let (new_stack, s_insts) = save_sregs 7 new_stack [] in
    let new_insts = (insts @ arg_insts @ narg_insts @ fp_insts @ ra_insts @ s_insts) in
    (new_stack, new_insts)


(* Function epilogue generation *)
let generate_epilogue (stack : virtualStack) : virtualStack * inst list =

    (* Restore Callee saved registers: $fp, $ra, and $s0-$s7 ($16-$23) *) 
    let rec load_sregs (num : int) (t_insts : inst list) =
        if num < 0 
        then t_insts
        else
            let name = "S"^(string_of_int num) in
            load_sregs (num - 1) t_insts @ [ Lw((string2reg name), fp, (find_local_var name stack)); ] 
    in

    let s_insts = load_sregs 7 [] in
    let ra_fp_insts = [ Lw(ra, fp, (find_local_var "RA" stack));
                        Lw(fp, fp, (find_local_var "FP" stack)); ] in
    let jr_insts    = [ Jr(R31); ] in
    let new_insts   = s_insts @ ra_fp_insts @ jr_insts in 
    (* Reset the SP to our FP (frame pop) *)
    (stack, new_insts)

(* Factors out common code for compiling two nested expressions and
 * carrying out some instruction. The result of e1 is stored in R3,
 * the result of e2 in R2. in is the instruction to carry out on these
 * results *)

(* Returns assembly code to store var from stack *)
let store_var (stack: virtualStack) (v: string) (dest: Mips.reg) : inst = 
    Sw(dest, fp, (find_local_var v stack))

(* Returns assembly code to load var from stack *)
let load_var (stack: virtualStack) (v: string) (dest: Mips.reg) : inst = 
    Lw(dest, fp, (find_local_var v stack))

(* Places result in R2 *)
let rec compile_exp_r (is: RInstList.rlist) ((e,_): Cish_ast.exp) (stack : virtualStack) : virtualStack * RInstList.rlist =

    (* HELPER: Compiles e1 and e2. Result of e1 goes in R2, e2 in R3 *)
    let dual_op (e1: Cish_ast.exp) (e2: Cish_ast.exp) (instructions: inst list) : virtualStack * RInstList.rlist =
        let (t, stack1, insts1) = new_temp stack in
        (* Compile e2 first so result goes in R3, getting resulting instructions and stack *)
        let (stack2, insts2) = compile_exp_r (is <@ insts1) e2 stack1 in
        (* Store result of e2; compile e1 *)
        let (stack3, insts3) = compile_exp_r (insts2 <@ [(store_var stack2 t R2)]) e1 stack2 in
        (* Load e2 into R3 and execute instruction *)
        let insts4 = insts3 <@ [(load_var stack3 t R3)] <@ instructions in
        (* Pop temp var *)
        let (stack4, pop_inst) = pop_local_var t stack3 in
            (stack3, insts4 <@ pop_inst) in

        match e with
            | Var v -> (stack, is <@ [(load_var stack v R2)]) (* Load from the correct stack offset *)
            | Int i -> (stack, is <@ [Li(R2, Word32.fromInt i)])
            | Binop(e1,op,e2) ->
                  let oper = (match op with 
                                  | Plus  -> Mips.Add(R2, R2, Reg(R3))
                                  | Minus -> Mips.Sub(R2, R2, R3)
                                  | Times -> Mips.Mul(R2, R2, R3)
                                  | Div   -> Mips.Div(R2, R2, R3)
                                  | Eq    -> Mips.Seq(R2, R2, R3)
                                  | Neq   -> Mips.Sne(R2, R2, R3)
                                  | Lt    -> Mips.Slt(R2, R2, Reg(R3))
                                  | Lte   -> Mips.Sle(R2, R2, R3)
                                  | Gt    -> Mips.Sgt(R2, R2, R3)
                                  | Gte   -> Mips.Sge(R2, R2, R3)) in
                      dual_op e1 e2 [oper]
            (* If R2 = 0, then set R2 = 1, else R2 = 0 *)
            | Not(e) -> let (stack1, insts1) = compile_exp_r is e stack in
                  (stack1, insts1 <@ [Mips.Seq(R2, R2, R0)])
            | And(e1, e2) -> 
                  dual_op e1 e2 
                      [
                          (* If R2 = 0, then R2 = 0, else R2 = 1 *)
                          Mips.Sne(R2, R2, R0);
                          (* If R3 = 0, then R3 = 0, else R3 = 1 *)
                          Mips.Sne(R3, R3, R0);
                          (* If R2 = R3 = 1, then R2 = 1, else R2 = 0 *)
                          Mips.And(R2, R2, Reg R3)
                      ]
            | Or(e1, e2) ->
                  dual_op e1 e2 
                      [
                          (* If R2 = 0, then R2 = 0, else R2 = 1 *)
                          Mips.Sne(R2, R2, R0);
                          (* If R3 = 0, then R3 = 0, else R3 = 1 *)
                          Mips.Sne(R3, R3, R0);
                          (* If R2 or R3 = 1, then R2 = 1, else R2 = 0 *)
                          Mips.Or(R2, R2, Reg R3)
                      ]
            (* Assumes variable has already been delcared; if not allows exception to fall through *)
            | Assign(v, e) -> 
                  let (stack1, insts1) = compile_exp_r is e stack in
                      (stack1, insts1 <@ [(store_var stack1 v R2)])
            | Call (f_exp, exp_list) -> raise TODO
            | Load (e) -> 
                  let (stack1, insts1) = compile_exp_r is e stack in
                  (stack1, insts1 <@ [Lw(R2, R2, Int32.zero)])
            | Store (dest, e) ->
                  (* Compile value and address, then store value at address *)
                  dual_op e dest [Sw(R2, R3, Int32.zero)]
            | Malloc (e) -> raise TODO

and compile_call f exp_list (stack : virtualStack) (prev_insts: RInstList.rlist) : virtualStack * RInstList.rlist =
    let arg_offset = Int32.of_int ((List.length exp_list) * -4) in

    (* helper to deal with groups of args *)
    let rec compile_call_r argno exps t_stack t_insts =
        (* For each expression *)
        match exps with 
        | []          -> (t_stack, t_insts <@ [ Jal(f); ]) (* Jump and Link *)
        | t_exp::rest -> 
            (* Setup Sandbox *)
            let sandbox_stack = { contents = t_stack.contents; 
                                  last_offset = (Int32.add t_stack.last_offset arg_offset); }
            in                     
            (* Compile expression *)
            let (sandbox_stack, new_insts) = compile_exp_r 
                                                    t_insts 
                                                    t_exp 
                                                    sandbox_stack 
            in

            (* Exit Sandbox *)
            (* Move result into aX OR If n_arg > 3 then push arg into next frame (past sp) *)
            let mv_insts = (if argno < 4 
                            then [ Add((string2reg ("A"^(string_of_int argno))), R2, Reg(R0));  ]
                            else [] ) @ [ Sw(R2, sp, (Int32.mul (-4l) (Int32.of_int argno)));   ]
            in
            compile_call_r (argno + 1) rest t_stack ( new_insts <@ mv_insts )
    in
    compile_call_r 0 exp_list stack prev_insts

(* Compiles a statement in reverse order *)
let rec compile_stmt_r (is: RInstList.rlist) ((s,pos): Cish_ast.stmt) (stack : virtualStack) : virtualStack * RInstList.rlist =
    match s with
         (* Using compile_exp_r directly eliminates redundant reversing the list *)
        | Exp e -> compile_exp_r is e stack
        | Let(t_var, t_exp, t_stmt) -> 
            (* Push a variable on to the stack *)
            let (stack1, insts1) = add_local_var t_var stack in
            (* Compile the expression *)
            let (stack2, insts2)  = compile_exp_r (is <@ insts1) t_exp stack1 in
            (* Store the expression in the var *)
            let sw_insts = [(store_var stack2 t_var R2)] in
            (* Compile the statment *)
            let (stack3, insts3) = compile_stmt_r (insts2 <@ sw_insts) t_stmt stack2 in
            (* Pop the variable *)
            let (stack4, pop_insts) = pop_local_var t_var stack3 in
            (stack4, insts3 <@ pop_insts)
        | Seq (s1, s2) ->
              let(stack1, insts1) = compile_stmt_r is s1 stack in
                  compile_stmt_r insts1 s2 stack1
        | If(e, then_s, else_s) ->
              (* Labels *)
              let else_l = new_label () in
              let end_l  = new_label () in
              (* Compile expression e*)
              let (stack1, insts1) = compile_exp_r is e stack in
              (* Branch is e1 evaluates to false *)
              let branch_inst = [Beq(R2,R0,else_l)] in
              (* Compile then_s *)
              let (stack2, insts2) = compile_stmt_r (insts1 <@ branch_inst) then_s stack1 in
              (* Jump to end, place else label *)
              let else_inst = [J(end_l); Label(else_l)] in
              (* Compile else_s *)
              let (stack3, insts3) = compile_stmt_r (insts2 <@ else_inst) else_s stack2 in
              (* Append end label *)
                  (stack3, insts3 <@ [Label(end_l)])
        | While(e, s) ->
              (* Labels *)
              let test_l = new_label () in
              let top_l  = new_label () in
              (* Jump and top label *)
              let head_inst = [J(test_l); Label(top_l)] in
              (* Compile statement *)
              let (stack1, insts1) = compile_stmt_r (is <@ head_inst) s stack in
              (* Test label instruction *)
              let test_inst = [Label(test_l)] in
              (* Compile expression *)
              let (stack2, insts2) = compile_exp_r (insts1 <@ test_inst) e stack1 in
              (* Append branch if expression is false *)
                  (stack2, insts2 <@ [Bne(R2,R0,top_l)])
        (* Transform for loops into while loops *)
        | For(e1, e2, e3, s) ->
              (* Helper to get position out of statement *)
              let get_pos s = let (_,p) = s in p in 
              (* Nastiness due to necesity of having position informaiton *)
              compile_stmt_r is ((Cish_ast.Seq(
                                      (Cish_ast.Exp e1, (get_pos e1)),
                                         (While(
                                              e2,
                                              (Cish_ast.Seq(s, (Cish_ast.Exp e3, (get_pos e3))), get_pos s)),
                                          pos))),
                                 pos) stack
        | Return (e) ->
              compile_exp_r is e stack
             
(* compiles a Fish statement down to a list of MIPS instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in R2 and then doing a Jr R31.
 *)
let compile_stmt (s : Cish_ast.stmt) (stack : virtualStack) : virtualStack * inst list = 
let(stack1, insts1) = compile_stmt_r RInstList.empty s stack in
    (stack1, RInstList.to_list insts1)

let compile_function (f : func) : inst list =
    let Fn(signature) = f in
    (* Allocate a local "stack" (Map) to simulate the real stack *)
    let local_stack = { last_offset = 0l; contents = StringMap.empty } in

        (* Generate a label for the function *)
        let f_label = Label((sanitize_f_name signature.name)) in

        (* Generate a prologue for the function *)
        let (new_stack, prologue_code) = generate_prologue signature local_stack in

        (* Code gen for the function *)
        let (new_stack, body_code) = compile_stmt signature.body new_stack in

        (* Generate an epilogue for the function *)
        let (new_stack, epilogue_code) = generate_epilogue new_stack in

        (* Concate code blocks together *)
        ([ f_label; ] @ prologue_code @ body_code @ epilogue_code)

let rec compile (p:Cish_ast.program) : result =
    let rec compile_prog (prog : Cish_ast.program) (compiled : result) =
        match prog with 
        | [] -> compiled
        | f::rest -> 
            let new_insts = compile_function f in
            compile_prog rest { code = compiled.code @ new_insts; data = compiled.data }
    in compile_prog p { code = []; data = [] }

let result2string (res:result) : string = 
    let code = res.code in
    let data = res.data in
    let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
    let vaR8decl x = x ^ ":\t.word 0\n" in
    let readfile f =
        let stream = open_in f in
        let size = in_channel_length stream in
        let text = String.create size in
        let _ = really_input stream text 0 size in
        let _ = close_in stream in 
        text in
        let debugcode = readfile "print.asm" in
            "\t.text\n" ^
            "\t.align\t2\n" ^
            "\t.globl main\n" ^
            (String.concat "" strs) ^
            "\n\n" ^
            "\t.data\n" ^
            "\t.align 0\n"^
            (String.concat "" (List.map vaR8decl data)) ^
            "\n" ^
            debugcode
