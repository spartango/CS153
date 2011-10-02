(* Compile Fish AST to MIPS AST *)
open Mips
open Ast

exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int()   = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

(* sets of variables -- Ocaml Set and Set.S *)
module VarSet = Set.Make(struct
                           type t = Ast.var
                           let compare = String.compare
                         end)

(* a table of variables that we need for the code segment *)
let variables : VarSet.t ref = ref (VarSet.empty)

let add_var (v: string) : unit =
    variables := VarSet.add v (!variables); ()

let find_var (v: string) : bool = 
    VarSet.exists (fun x -> x = v) !variables 

(* generate a fresh temporary variable and store it in the variables set. *)
let rec new_temp() : string= 
    let t = "T" ^ (string_of_int (new_int())) in
    (* make sure we don't already have a variable with the same name! *)
    if VarSet.mem t (!variables) then new_temp()
    else let _ = add_var t in t

(* reset internal state *)
let reset() = (label_counter := 0; variables := VarSet.empty)

(* find all of the variables in a program and add them to
 * the set variables *)
let rec collect_vars (p : Ast.program) : unit = 
    let stip_pos r = let(v,_) = r in v in
    let rec collect_vars_e (e: Ast.exp) : unit =
        match (stip_pos e) with
            | Var v -> add_var ("V"^v)
            | Assign (v, e1) ->
                  let _ = add_var ("V"^v) in collect_vars_e e1
            | Int _ -> ()
            | Binop(e1, _, e2) -> collect_vars_e e1;
                  collect_vars_e e2
            | Not e -> collect_vars_e e;
            | And (e1, e2) -> collect_vars_e e1;
                  collect_vars_e e2
            | Or (e1, e2) -> collect_vars_e e1;
                  collect_vars_e e2 
    in
    match (stip_pos p) with
        | Exp e -> collect_vars_e e
        | Seq (s1, s2) -> collect_vars s1;
              collect_vars s2
        | If (e, s1, s2) -> collect_vars_e e;
              collect_vars s1;
              collect_vars s2
        | While (e, s) -> collect_vars_e e;
              collect_vars s
        | For (e1, e2, e3, s) -> collect_vars_e e1;
              collect_vars_e e2;
              collect_vars_e e3;
              collect_vars s
        | Return e -> collect_vars_e e

(* Prepends reversed x onto accum. Order of parameters for 
 * readability of code *)
let rec revapp (accum: 'a list) (x: 'a list) : 'a list=
    match x with
        | []         -> accum
        | head::tail -> revapp (head::accum) tail

let rev x = revapp [] x

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
                  | Plus  -> Add(R2, R3, Reg(R2))
                  | Minus -> Sub(R2, R3, R2)
                  | Times -> Mul(R2, R3, R2)
                  | Div   -> Mips.Div(R2, R3, R2)
                  | Eq    -> Mips.Seq(R2, R3, R2)
                  | Neq   -> Sne(R2, R3, R2)
                  | Lt    -> Slt(R2, R3, R2)
                  | Lte   -> Sle(R2, R3, R2)
                  | Gt    -> Sgt(R2, R3, R2)
                  | Gte   -> Sge(R2, R3, R2)) in
                  dual_op e1 e2 oper
        (* If R3 = 0, then set R2 = 1, else R2 = 0 *)
        | Not(e) -> revapp (compile_exp_r is e) [Mips.Seq(R2, R3, R0)]
        | And(e1, e2) -> 
              dual_op e1 e2 (Mips.And(R2, R2, Reg R3))
        | Or(e1, e2) ->
              dual_op e1 e2 (Mips.Or(R2, R2, Reg R3))
        | Assign(v, e) -> revapp (compile_exp_r is e) [La(R3, "V"^v); Sw(R2,R3, Int32.zero)] 

(* Compiles a statement in reverse order *)
let rec compile_stmt_r (is: inst list) ((s,pos): Ast.stmt)  : inst list =
    match s with
         (* Using compile_exp_r directly eliminates redundant reversing the list *)
        | Exp e -> compile_exp_r is e
        | Seq (s1, s2) ->
              compile_stmt_r (compile_stmt_r is s1) s2
        | If(e, then_s, else_s) ->
              (* Test e, branch to else_s if not equal *)
              let else_l = new_label () in
              let end_l  = new_label () in
              revapp (compile_exp_r is e) 
                  (revapp 
                          (compile_stmt_r 
                            (revapp 
                                   (compile_stmt_r [Beq(R2,R0,else_l)] then_s)
                                   [J(end_l); Label(else_l)]
                            )
                            else_s)
                          [Label(end_l)])
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

(* compiles Fish AST down to MIPS instructions and a list of global vars *)
let compile (p : Ast.program) : result = 
    let _ = reset() in
    let _ = collect_vars(p) in
    let insts = (Label "main") :: (compile_stmt p) in
    { code = insts; data = VarSet.elements (!variables) }

let code_to_string code = 
  List.map (fun x -> (Mips.inst2string x) ^ "\n") code

(* converts the output of the compiler to a big string which can be 
 * dumped into a file, assembled, and run within the SPIM simulator
 * (hopefully). *)
let result2string ({code;data}:result) : string = 
    let strs = code_to_string code in
    let var2decl x = x ^ ":\t.word 0\n" in
    "\t.text\n" ^
    "\t.align\t2\n" ^
    "\t.globl main\n" ^
    (String.concat "" strs) ^
    "\n\n" ^
    "\t.data\n" ^
    "\t.align 0\n"^
    (String.concat "" (List.map var2decl data)) ^
    "\n"

