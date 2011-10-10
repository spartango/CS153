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

(* Offset is with respect to the Frame Pointer (fp) *)
type VirtualStack = {  last_offset : int32; 
                       contents    : StringMap }

(* Code Gen *)

(* Function prologue generation *)
let generate_prologue (stack : VirtualStack) : VirtualStack * inst list =
    (* Set new FP *)
    let insts = [ Sw( Utility.fp, Utility.sp, -4); 
                  Add(Utility.fp, Utility.sp, 0); ]
    in
    (* Save Callee saved registers: $fp, $ra, and $s0-$s7 ($16-$23) *)
    let (new_stack, fp_insts) =  add_local_var "FP" stack in
    let (new_stack, ra_insts) =  add_local_var "RA" new_stack in
    let ra_insts = ra_insts @ [ Sw(Utility.ra, Utility.fp, (find_local_var "RA" new_stack)); ] in

    let rec save_sregs (num : int) (t_stack : VirtualStack) (t_insts : inst list) =
        if num < 0 
        then (t_stack, t_insts) 
        else 
            let name = "S"^(string_of_int num) in
            let (new_stack, s_insts) = add_local_var name t_stack in
            let new_insts = s_insts @ [ Sw((string2reg name), Utility.fp, (find_local_var name)); ] in
            save_sregs (num - 1) new_stack (t_insts @ new_insts)
    in 

    let (new_stack, s_insts) = save_sregs 7 new_stack [] in
    let new_insts = (insts @ fp_insts @ ra_insts @ s_insts) in
    (new_stack, new_insts)


(* Function epilogue generation *)
let generate_epilogue (stack : VirtualStack) : VirtualStack * inst list =

    (* Restore Callee saved registers: $fp, $ra, and $s0-$s7 ($16-$23) *) 
    let rec load_sregs (num : int) (t_insts : inst list) =
        if num < 0 
        then t_insts
        else
            let name = "S"^(string_of_int num) in
            load_sregs (num - 1) t_insts @ [ Lw((string2reg name), Utility.fp, (find_local_var name)); ] 
    in

    let s_insts = load_sregs 7 stack [] in
    let ra_fp_insts = [ Lw(Utility.ra, Utility.fp, (find_local_var "RA"));
                        Lw(Utility.fp, Utility.fp, (find_local_var "FP")); ] in
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
    let insts = [ Add(Utility.sp, Utility.sp, -4l); ] in
    (new_stack, insts)

(* Generates code to pop a variable off the stack *)
let pop_local_var (v : string) (stack : VirtualStack) : VirtualStack * inst list =
    let new_contents = Map.remove v stack.contents in
    let new_stack = { last_offset = (Int32.add stack.last_offset 4l) ; contents = new_contents } in
    let insts = [ Add(Utility.sp, Utility.sp, 4l); ] in
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
    Sw(dest, Utility.fp, (find_local_var v stack))

(* Returns assembly code to load var from stack *)
let load_var (stack: VirtualStack) (v: string) (dest: Mip.reg) : inst = 
    Lw(dest, Utility.fp, (find_local_var v stack))

(* Places result in R2 *)
let rec compile_exp_r (is: RInstList.rlist) ((e,_): Ast.exp) (stack : VirtualStack) : VirtualStack * inst list =

    (* HELPER: Compiles e1 and e2. Result of e1 goes in R2, e2 in R3 *)
    let dual_op (e1: Ast.exp) (e2: Ast.exp) (instruction: inst) : VirtualStack * RL.rlist =
        let (t, stack1, insts1) = new_temp() in
        (* Compile e2 first so result goes in R3, getting resulting instructions and stack *)
        let (stack2, insts2) = compile_exp_r (is <@ insts0) e2 stack0 in
        (* Store result of e2; compile e1 *)
        let (stack3, insts3) = compile_exp_r (insts1 <@ [(store_var stack1 t R2)]) e1 stack1 in
        (* Load e2 into R3 and execute instruction *)
        let insts4 = insts2 <@ [(load_var stack 2 t R3; instruction] in
        (* Pop temp var *)
        let (stack4, pop_inst) = pop_local_var t stack2 in
            (stack3, insts3 <@ pop_inst) in

        match e with
            | Var v -> is <@ [(load_var v stack R2)] (* Load from the correct stack offset *)
            | Int i -> is <@ [Li(R2, Word32.fromInt i)]
            | Binop(e1,op,e2) ->
                  let oper = (match op with 
                                  | Plus  -> Mips.Add(R2, R2, Reg(R3))
                                  | Minus -> Mips.Sub(R2, R2, R3)
                                  | Times -> Mips.Mul(R2, R2, R3)
                                  | Div   -> Mips.Div(R2, R2, R3)
                                  | Eq    -> Mips.Seq(R2, R2, R3)
                                  | Neq   -> Mips.Sne(R2, R2, R3)
                                  | Lt    -> Mips.Slt(R2, R2, R3)
                                  | Lte   -> Mips.Sle(R2, R2, R3)
                                  | Gt    -> Mips.Sgt(R2, R2, R3)
                                  | Gte   -> Mips.Sge(R2, R2, R3)) in
                      dual_op e1 e2 oper
            (* If R3 = 0, then set R2 = 1, else R2 = 0 *)
            | Not(e) -> let (stack1, insts1) = (compile_exp_r is e stack)
                  (stack1, insts1 <@ [Mips.Seq(R2, R3, R0)])
            | And(e1, e2) -> 
                  dual_op e1 e2 (Mips.And(R2, R2, Reg R3))
            | Or(e1, e2) ->
                  dual_op e1 e2 (Mips.Or(R2, R2, Reg R3))
            (* Assumes variable has already been delcared; if not allows exception to fall through *)
            | Assign(v, e) -> 
                  let (stack1, insts1) = compile_expr_r is e stack in
                      (stack1, insts1 <@ [(store_var stack1 v R2)])
            | Call(f, exp_list) ->
                  raise TODO

(* Compiles a statement in reverse order *)
let rec compile_stmt_r (is: inst list) ((s,pos): Ast.stmt) (stack : VirtualStack) : VirtualStack * inst list =
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
            let (stack3, insts3) = compile_stmt (insts2 <@ sw_insts) t_stmt stack2 in
            (* Pop the variable *)
            let (stack4, pop_insts)  = pop_local_var t_var new_stack in
            (new_stack, insts4 <@ pop_insts)
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
              compile_stmt_r is ((Ast.Seq(
                                      (Ast.Exp e1, (get_pos e1)),
                                         (While(
                                              e2,
                                              (Ast.Seq(s, (Ast.Exp e3, (get_pos e3))), get_pos s)),
                                          pos))),
                                 pos)
        | Return (e) ->
              (compile_exp_r is e stack) <@ [Jr(R31)] 
             
(* compiles a Fish statement down to a list of MIPS instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in R2 and then doing a Jr R31.
 *)
let compile_stmt (s : Ast.stmt) (stack : VirtualStack) : VirtualStack * inst list = 
    rev (compile_stmt_r RInstList.empty s stack)

let compile_function (f : func) : inst list = 
    let Fn(signature) = f in
    (* Allocate a local "stack" (Map) to simulate the real stack *)
    let local_stack = { last_offset = 0; contents = StringMap.empty } in

        (* Generate a label for the function *)
        let f_label = Label(signature.name) in

        (* Generate a prologue for the function *)
        let (new_stack, prologue_code) = generate_prologue local_stack in

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
