(* Compile Fish AST to MIPS AST *)
open Mips
open Ast

exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
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
    (*************************************************************)
    let stip_pos r = let(v,_) = r in v in
    let rec collect_vars_e (e: Ast.exp) : unit =
        match (stip_pos e) with
            | Var v -> add_var v
            | Assign (v, e1) -> add_var v
            | Int _ -> ()
            | Binop(e1, _, e2) -> collect_vars_e e1;
                  collect_vars_e e2
            | Not e -> collect_vars_e e;
            | And (e1, e2) -> collect_vars_e e1;
                  collect_vars_e e2
            | Or (e1, e2) -> collect_vars_e e1;
                  collect_vars_e e2 in
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
    (*************************************************************)

(* Appends x onto the end of lst. lst is a reversed list*)
let rec revapp (x: 'a list) (lst: 'a list) : 'a list=
    match x with
        | [] -> lst
        | head::tail -> revapp tail (head::lst)

let rev x = revapp x []

let rec compile_exp_r ((e,_): Ast.exp) (is: inst list) : inst list = 
    match e with
        | Var v -> revapp [La(R2, v); Lw(R2,R2, Int32.zero)] is
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
              let t = new_temp() in
                  (* This list must be reversed *)
                  revapp [oper] (revapp (compile_exp_r e2 []) 
                                     (revapp [La(R3,t); Lw(R3,R3, Int32.zero)]
                                          (revapp [La(R3,t); Sw(R2,R3, Int32.zero)]
                                               (compile_exp_r e1 is))))
        (* If R3 = 0, then set R2 = 1, else R2 = 0 *)
        | Not(e1) -> compile_exp_r e1 (Mips.Seq(R2, R3, R0)::is)
        | And(e1, e2) -> 
              let t = new_temp() in
                  revapp [La(R3, t); Lw(R3, R3, Int32.zero); 
                          Mips.And(R2, R3, Reg R2)] 
                      (compile_exp_r e2
                           (revapp [La(R3, t); Sw(R2, R3, Int32.zero)]
                                     (compile_exp_r e1 is)))

        | Or(e1, e2) -> revapp [Mips.Or(R2, R3, Reg R2)] 
                                     (revapp (compile_exp_r e2 []) 
                                     (compile_exp_r e1 is))
        | Assign(v, e) -> revapp [Sw(R2,R3, Int32.zero)] 
              (compile_exp_r e (revapp [La(R3, v)] is))

(*
              let oper = (match op with
                  (* Reg for third parameter b/c operand definition *)
                  | Plus  -> Add(R2, R2, Reg(R3))
                  | Minus -> Sub(R2, R2, R3)
                  | Times -> Mul(R2, R2, R3)
                  | Div   -> Mips.Div(R2, R2, R3)) in
              let t = new_temp() in
                  compile_exp_r e1
                      (revapp [La(R3,t); Sw(R2,R3, Int32.zero)] 
                           (compile_exp_r e2
                                (revapp [La(R3,t); Lw(R3,R3, Int32.zero)]
                                (oper::is))))
*)

let compile_exp (e: Ast.exp) : inst list =
    rev (compile_exp_r e [])

let rec compile_stmt_r ((s,_): Ast.stmt) (is: inst list) : inst list =
    match s with
        | Exp e -> revapp (compile_exp e) is
        | Seq (s1, s2) ->
              compile_stmt_r s2 (compile_stmt_r s1 is)
        | Return (e) ->
              revapp [Jr(R31)] 
                  (revapp (compile_exp e) is)
        | _ -> raise IMPLEMENT_ME

(* compiles a Fish statement down to a list of MIPS instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in R2 and then doing a Jr R31.
 *)
let compile_stmt (s :Ast.stmt) : inst list = 
    rev (compile_stmt_r s [])

(* compiles Fish AST down to MIPS instructions and a list of global vars *)
let compile (p : Ast.program) : result = 
    let _ = reset() in
    let _ = collect_vars(p) in
    let insts = (Label "main") :: (compile_stmt p) in
    { code = insts; data = VarSet.elements (!variables) }

(* converts the output of the compiler to a big string which can be 
 * dumped into a file, assembled, and run within the SPIM simulator
 * (hopefully). *)
let result2string ({code;data}:result) : string = 
    let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
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

