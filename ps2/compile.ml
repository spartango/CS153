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
        | _ -> raise IMPLEMENT_ME
    (*************************************************************)

(* compiles a Fish statement down to a list of MIPS instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in R2 and then doing a Jr R31.
 *)
let rec compile_stmt ((s,_):Ast.stmt) : inst list = 
    (*************************************************************)
    raise IMPLEMENT_ME
    (*************************************************************)

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

