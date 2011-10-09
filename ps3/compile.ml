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
type VirtualStack = {  last_offset : int; 
                       contents    : StringMap }

(* Code Gen *)

(* Function prologue generation *)
let generate_prologue (stack : VirtualStack) : VirtualStack * inst list =
    (* Save Callee saved registers: $fp, $ra, and $s0-$s7 ($16-$23) *)
    (* Set new FP *)
    raise TODO

(* Function epilogue generation *)
let generate_epilogue (stack : VirtualStack) : VirtualStack * inst list =
    (* Restore Callee saved registers: $fp, $ra, and $s0-$s7 ($16-$23) *)
    (* Reset the SP to our FP (frame pop) *)
    raise TODO

(* Generates code to push a variable on to the stack *)
let add_local_var (v : string) (stack : VirtualStack) : VirtualStack * inst list =
    (* Push variable on to stack *)
    (* Variable is an aligned 32 bit int *)
    let new_contents = Map.add v stack.last_offset stack.contents in
    let new_stack = { last_offset = stack.last_offset + 4 ; contents = new_contents } in
    (* Generate corresponding instructions *)
    (* Move $sp *)
    let insts = [ Add(SP, SP, -4); ] in
    (new_stack, insts)

(* Generates code to pop a variable off the stack *)
let pop_local_var (v : string) (stack : VirtualStack) : VirtualStack * inst list =
    let new_contents = Map.remove v stack.contents in
    let new_stack = { last_offset = stack.last_offset - 4 ; contents = new_contents } in
    let insts = [ Add(SP, SP, 4); ] in
    (new_stack, insts)

(* Provides the offset of a variable relative to the stack ptr *)
let find_local_var (v : string) (stack : VirtualStack) : int = 
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

(* Returns assembly code to load var from stack *)
let load_var (s: VirtualStack) (v: string) (dest: Mip.reg) : inst = 
    Lw(dest, 

let rec compile_exp_r ((e,_): Ast.exp) (stack : VirtualStack) : VirtualStack * inst list =

    (* Load result of first expression and carry out instruction *)
    let dual_op (e1: Ast.exp) (e2: Ast.exp) (instruction: inst) : inst list =
        let t = new_temp() in
            revapp (compile_exp_r 
                        (revapp (compile_exp_r is e1) [ (* TODO: do a lookup here *) Sw(R2, R3, Int32.zero)])
                        e2)
                [(* TODO: do a lookup here *) Lw(R3, R3, Int32.zero); instruction] 
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
        (* *)
        raise TODO

(* Compiles a statement in reverse order *)
let rec compile_stmt_r (is: inst list) ((s,pos): Ast.stmt) (stack : VirtualStack) : VirtualStack * inst list =
    match s with
         (* Using compile_exp_r directly eliminates redundant reversing the list *)
        | Exp e -> compile_exp_r is e
        | Let(t_var, t_exp, t_stmt) -> 
            (* Push a variable on to the stack *)
            (* Code gen the statement *)
            (* Pop the stack *)
            raise TODO 
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
        let (new_stack, prologue_code) = generate_prologue local_stack in

        (* Code gen for the function *)
        let (new_stack, body_code) = compile_stmt signature.body new_stack in

        (* Generate an epilogue for the function *)
        let (new_stack, epilogue_code) = generate_epilogue new_stack in

        (* Concate code blocks together *)

    raise TODO

let rec compile (p:Ast.program) : result =
    (* For each function *)
    (* Compile function *)
    raise TODO

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
