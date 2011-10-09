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

(* Code Gen *)

(* Function prologue generation *)

(* Function epilogue generation *)

let add_var (v: string) : unit =
    (* Push variable on to stack *)
    raise TODO

let rec new_temp() : string= 
    (* Create a variable, add it *)
    raise TODO

(* Factors out common code for compiling two nested expressions and
 * carrying out some instruction. The result of e1 is stored in R3,
 * the result of e2 in R2. in is the instruction to carry out on these
 * results *)
let rec compile_exp_r (is: inst list) ((e,_): Ast.exp): inst list =
    let dual_op (e1: Ast.exp) (e2: Ast.exp) (instruction: inst) : inst list =
        let t = new_temp() in
            (* Load result of first expression and carry out instruction *)
            revapp (compile_exp_r 
                        (revapp (compile_exp_r is e1) [La(R3, t); Sw(R2, R3, Int32.zero)])
                        e2)
                [La(R3, t); Lw(R3, R3, Int32.zero); instruction] in  
        match e with
        | Var v -> revapp is [La(R2, "V"^v); Lw(R2,R2, Int32.zero)]
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
        | Assign(v, e) -> revapp (compile_exp_r is e) [La(R3, "V"^v); Sw(R2,R3, Int32.zero)] 
        | Call(f, exp_list) -> 
            (* Follow calling conventions to invoke a function, setting up a new frame for it etc *)
            raise TODO

(* Compiles a statement in reverse order *)
let rec compile_stmt_r (is: inst list) ((s,pos): Ast.stmt)  : inst list =
    match s with
         (* Using compile_exp_r directly eliminates redundant reversing the list *)
        | Exp e -> compile_exp_r is e
        | Let(t_var, t_exp, t_stmt) -> 
            (* Push a variable on to the stack, exec the statement, then pop it *)
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
let compile_stmt (s :Ast.stmt) : inst list = 
    rev (compile_stmt_r [] s)

let rec compile (p:Ast.program) : result =
    (* For each function *)
    (* Generate a label for the function *)
    (* Generate a prologue for the function *)
    (* Code gen for the function *)
    (* Generate an epilogue for the function *)
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
