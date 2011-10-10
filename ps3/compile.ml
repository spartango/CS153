(* Compile Cish AST to MIPS AST *)
open Mips
open Ast
open Utility

exception TODO

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

(* Stack Manipulation *)

(* Offset is with respect to the Frame Pointer (FP) *)
type VirtualStack = {  last_offset : int32; 
                       contents    : StringMap }

(* Code Gen *)

(* Function prologue generation *)
let generate_prologue (f_sig : funcsig) (stack : VirtualStack) : VirtualStack * inst list =
    let n_args = List.length f_sig.args in
    let arg_offset = (Int32.mul -4l (Int32.of_int n_args)) in

    (* Save the old FP and set new FP *)
    let insts = [ Sw(FP, SP, arg_offset); 
                  Add(FP, SP, 0l); ]
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

    (* Mark the stack positions of arguments *)
    (* First are n_arg > 3 *)
    let (new_stack, arg_insts) = if (arg_offset < -16l) 
                                 then (stack, []) 
                                 else mark_high_args 0 f_sig.args stack []
    in
    
    let rec save_low_args touched_num arg_names t_stack t_insts =
        if touched_num >= 4 
        then (t_stack, t_insts)
        else
            match arg_names with
            | []       -> (t_stack, t_insts)
            | hd::rest -> let (new_stack, new_insts) = add_local_var hd t_stack in
                          save_low_args (touched_num + 1) rest new_stack (t_insts @ new_insts)
    in 

    (* Then the first 4 args *)
    (* Save the rest of the arguments a0 - a3 *)
    let (new_stack, narg_insts) = save_low_args 0 f_sig.args new_stack []
    in

    (* Mark the Framepointer *)
    let (new_stack, fp_insts) =  add_local_var "FP" new_stack in


    (* Save Callee saved registers: $ra, and $s0-$s7 ($16-$23) *)
    let (new_stack, ra_insts) =  add_local_var "RA" new_stack in
    let ra_insts = ra_insts @ [ Sw(RA, FP, (find_local_var "RA" new_stack)); ] in

    let rec save_sregs (num : int) (t_stack : VirtualStack) (t_insts : inst list) =
        if num < 0 
        then (t_stack, t_insts) 
        else 
            let name = "S"^(string_of_int num) in
            let (new_stack, s_insts) = add_local_var name t_stack in
            let new_insts = s_insts @ [ Sw((string2reg name), FP, (find_local_var name)); ] in
            save_sregs (num - 1) new_stack (t_insts @ new_insts)
    in 

    let (new_stack, s_insts) = save_sregs 7 new_stack [] in
    let new_insts = (insts @ arg_insts @ narg_insts @ fp_insts @ ra_insts @ s_insts) in
    (new_stack, new_insts)


(* Function epilogue generation *)
let generate_epilogue (stack : VirtualStack) : VirtualStack * inst list =

    (* Restore Callee saved registers: $fp, $ra, and $s0-$s7 ($16-$23) *) 
    let rec load_sregs (num : int) (t_insts : inst list) =
        if num < 0 
        then t_insts
        else
            let name = "S"^(string_of_int num) in
            load_sregs (num - 1) t_insts @ [ Lw((string2reg name), FP, (find_local_var name)); ] 
    in

    let s_insts = load_sregs 7 stack [] in
    let ra_fp_insts = [ Lw(RA, FP, (find_local_var "RA"));
                        Lw(FP, FP, (find_local_var "FP")); ] in
    let new_insts = s_insts @ ra_fp_insts in 
    (* Reset the SP to our FP (frame pop) *)
    (stack, new_insts)

(* Generates code to push a variable on to the stack *)
let add_local_var (v : string) (stack : VirtualStack) : VirtualStack * inst list =
    (* Push variable on to stack *)
    (* Variable is an aligned 32 bit int *)
    let new_contents = Map.add v stack.last_offset stack.contents in
    let new_stack = { last_offset = (Int32.add stack.last_offset -4l) ; contents = new_contents } in
    (* Generate corresponding instructions *)
    (* Move $sp *)
    let insts = [ Add(SP, SP, -4l); ] in
    (new_stack, insts)

(* Generates code to pop a variable off the stack *)
let pop_local_var (v : string) (stack : VirtualStack) : VirtualStack * inst list =
    let new_contents = Map.remove v stack.contents in
    let new_stack = { last_offset = (Int32.add stack.last_offset 4l) ; contents = new_contents } in
    let insts = [ Add(SP, SP, 4l); ] in
    (new_stack, insts)

(* Provides the offset of a variable relative to the stack ptr *)
let find_local_var (v : string) (stack : VirtualStack) : int32 = 
    Map.find v stack

(* Generates code to create a new temporary var *)
let rec new_temp (stack : VirtualStack) : string * VirtualStack * inst list = 
    (* Create a variable, add it *)
    let name = "T"^(new_int ()) in
    let (new_stack, insts) = add_local_var name stack in
    (name, new_stack, insts)

(* Factors out common code for compiling two nested expressions and
 * carrying out some instruction. The result of e1 is stored in R3,
 * the result of e2 in R2. in is the instruction to carry out on these
 * results *)

(* Returns assembly code to store var from stack *)
let store_var (stack: VirtualStack) (v: string) (dest: Mip.reg) : inst = 
    Sw(dest, Utility.FP, (find_local_var v stack))

(* Returns assembly code to load var from stack *)
let load_var (stack: VirtualStack) (v: string) (dest: Mip.reg) : inst = 
    Lw(dest, Utility.FP, (find_local_var v stack))

let rec compile_exp_r ((e,_): Ast.exp) (stack : VirtualStack) : VirtualStack * inst list =

    (* Load result of first expression and carry out instruction *)
    let dual_op (e1: Ast.exp) (e2: Ast.exp) (instruction: inst) : inst list =
        let (t, stack, stack_inst) = new_temp() in
            (compile_exp_r ((compile_exp_r (is <@ stack_inst) e1) <@ [(store_var stack t R2)])
                        e2) <@  [(store_var stack t R3); instruction] 
    in  
    match e with
    | Var v -> raise TODO (* Load from the correct stack offset *)
    | Int i -> Li(R2, Word32.fromInt i)::is
    | Binop(e1,op,e2) ->
          let oper = (match op with 
              | Plus  -> Mips.Add(R2, R3, Reg(R2))
              | Minus -> Mips.Sub(R2, R3, R2)
              | Times -> Mips.Mul(R2, R3, R2)
              | Div   -> Mips.Div(R2, R3, R2)
              | Eq    -> Mips.Seq(R2, R3, R2)
              | Neq   -> Mips.Sne(R2, R3, R2)
              | Lt    -> Mips.Slt(R2, R3, Reg(R2))
              | Lte   -> Mips.Sle(R2, R3, R2)
              | Gt    -> Mips.Sgt(R2, R3, R2)
              | Gte   -> Mips.Sge(R2, R3, R2)) in
              dual_op e1 e2 oper
    (* If R3 = 0, then set R2 = 1, else R2 = 0 *)
    | Not(e) -> revapp (compile_exp_r is e) [Mips.Seq(R2, R3, R0)]
    | And(e1, e2) -> 
          dual_op e1 e2 (Mips.And(R2, R2, Reg R3))
    | Or(e1, e2) ->
          dual_op e1 e2 (Mips.Or(R2, R2, Reg R3))
    | Assign(v, e) -> 
        (* Check if the variable is already on the stack *)
        (* If its there, pull it up *)
        (* otherwise push a new variable *)
        revapp (compile_exp_r is e) [(* TODO: do a lookup here *) Sw(R2,R3, Int32.zero)] 
    | Call(f, exp_list) -> 
        (* Follow calling conventions to invoke a function *)
        (* Map arguments to expressions *)
        raise TODO

(* Compiles a statement in reverse order *)
let rec compile_stmt_r (is: inst list) ((s,pos): Ast.stmt) (stack : VirtualStack) : VirtualStack * inst list =
    match s with
         (* Using compile_exp_r directly eliminates redundant reversing the list *)
        | Exp e -> compile_exp_r is e
        | Let(t_var, t_exp, t_stmt) -> 
            (* Push a variable on to the stack *)
            let (new_stack, push_insts) = add_local_var t_var stack in
            (* Compile the expression *)
            let (new_stack, exp_insts)  = compile_exp_r is t_exp new_stack in
            (* Store the expression in the var *)
            let sw_insts = [Sw(R2, FP, (find_local_var t_var new_stack)); ] in
            (* Compile the statment *)
            let (new_stack, stmt_insts) = compile_stmt t_stmt new_stack in
            (* Pop the variable *)
            let (new_stack, pop_insts)  = pop_local_var t_var new_stack in
            (new_stack, (push_insts @ exp_insts @ sw_insts @ pop_insts)
        | Seq (s1, s2) ->
              compile_stmt_r (compile_stmt_r is s1) s2
        | If(e, then_s, else_s) ->
              (* Test e, branch to else_s if not equal *)
              let else_l = new_label () in
              let end_l  = new_label () in
              revapp (compile_exp_r is e) 
                     (rev (revapp 
                          (compile_stmt_r 
                            (revapp 
                                   (compile_stmt_r [Beq(R2,R0,else_l)] then_s)
                                   [J(end_l); Label(else_l)]
                            )
                            else_s)
                          [Label(end_l)]))
        | While(e, s) ->
              let test_l = new_label () in
              let top_l  = new_label () in
              revapp 
                  (compile_exp_r (
                       revapp 
                           (compile_stmt_r 
                           (revapp is [J(test_l); Label(top_l)]) 
                           s)
                           [Label(test_l)])
                       e)
                  [Bne(R2,R0,top_l)]
        (* Transform for loops into while loops *)
        | For(e1, e2, e3, s) ->
              (* Helper to get position out of statement *)
              let get_pos s = let (_,p) = s in p in 
              (* Nastiness due to necesity of having position informaiton *)
              compile_stmt_r is ((Ast.Seq(
                                      (Ast.Exp e1, (get_pos e1)),
                                         (While(
                                              e2,
                                              (Ast.Seq(s, (Ast.Exp e3, (get_pos e3))), get_pos s)),
                                          pos))),
                                 pos)
        | Return (e) ->
              revapp (compile_exp_r is e) [Jr(R31)] 
             
(* compiles a Fish statement down to a list of MIPS instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in R2 and then doing a Jr R31.
 *)
let compile_stmt (s : Ast.stmt) (stack : VirtualStack) : VirtualStack * inst list = 
    rev (compile_stmt_r [] s stack)

let compile_function (f : func) : inst list = 
    let Fn(signature) = f in
    (* Allocate a local "stack" (Map) to simulate the real stack *)
    let local_stack = { last_offset = 0; contents = StringMap.empty } in

        (* Generate a label for the function *)
        let f_label = Label(signature.name) in

        (* Generate a prologue for the function *)
        let (new_stack, prologue_code) = generate_prologue signature local_stack in

        (* Code gen for the function *)
        let (new_stack, body_code) = compile_stmt signature.body new_stack in

        (* Generate an epilogue for the function *)
        let (new_stack, epilogue_code) = generate_epilogue new_stack in

        (* Concate code blocks together *)
        ([ f_label; ] @ prologue_code @ body_code @ epilogue_code)

let rec compile (p:Ast.program) : result =
    let rec compile_prog (prog : Ast.program) (compiled : result) =
        match p with 
        | [] -> compiled
        | f::rest -> 
            let new_insts = compile_function f in
            compile_prog rest { code = compiled.code @ new_insts; compiled.data }
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
