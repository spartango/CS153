open Pretty_print
open Test_framework
open Ast
open Utility
open Compile

module SAst =
    struct
        type sexp = 
                Int of int
            | Var of var
            | Binop of sexp * binop * sexp
            | Not of sexp                          (* !x *)
            | And of sexp * sexp                    (* x < y && y < z *)
            | Or of sexp * sexp                     (* x < y || x < z *)
            | Assign of var * sexp                 (* x = y+42 *)
            | Call of var * (sexp list)            (* f(x,y,z) *)
                  
        type sstmt = 
                Exp of sexp                          (* x = 3+4; *)
            | Seq of sstmt * sstmt                  (* x = 2*9; y = 42; *)
            | If of sexp * sstmt * sstmt             (* if (x == y) x = 42 else y = 43 *)
            | While of sexp * sstmt                 (* while (x < y) x = x + 1; *)
            | For of sexp * sexp * sexp * sstmt       (* for (x=0; x<y; x=x+1) y=y*42; *)
            | Return of sexp                       (* return e; *)
            | Let of var * sexp * sstmt             (* let x=3; in x=x+1; *)


        let rec transform_e (e: sexp) : Ast.exp =
            match e with
                | Int(i) -> (Ast.Int(i), 0)
                | Var(v) -> (Ast.Var(v), 0)
                | Binop(e1, binop, e2) ->
                      (Ast.Binop((transform_e e1),binop, (transform_e e2)), 0)
                | Not(e) -> (Ast.Not(transform_e e), 0)
                | And(e1,e2) -> 
                      (Ast.And((transform_e e1), (transform_e e2)), 0)
                | Or(e1, e2) ->
                      (Ast.And((transform_e e1), (transform_e e2)), 0)
                | Assign(v, e) ->
                      (Ast.Assign(v, transform_e e), 0)
                | Call(v, es) ->
                      (Ast.Call(v, (List.map transform_e es)), 0)
                          
        let rec transform (s: sstmt) : Ast.stmt =
            match s with
                | Exp(e) -> (Ast.Exp(transform_e e), 0)
                | Seq(s1, s2) ->
                      (Ast.Seq((transform s1), (transform s2)), 0)
                | If(e, s1, s2) ->
                      (Ast.If((transform_e e), (transform s1), (transform s2)), 0)
                | While(e, s) ->
                      (Ast.While((transform_e e), (transform s)), 0)
                | For(e1, e2, e3, s) ->
                      (Ast.For((transform_e e1), (transform_e e2), (transform_e e3),
                               (transform s)), 0)
                | Return(e) ->
                      (Ast.Return(transform_e e), 0)
                | Let(v, e, s) ->
                      (Ast.Let(v, (transform_e e), (transform s)), 0)
        let simple_op = 
            Exp(Binop(Int(3),Plus,Int(4)))

        let simple_compile_stmt (s: sstmt) : Compile.result = 
            let(_, asm) = compile_stmt (transform s) { last_offset = 0l; contents = StringMap.empty } in
                { code = asm; data = []}

        let res_to_str (res:result) : string = 
            let code = res.code in
            let data = res.data in
            let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
            let vaR8decl x = x ^ ":\t.word 0\n" in
                "\t.text\n" ^
                    "\t.align\t2\n" ^
                    "\t.globl main\n" ^
                    (String.concat "" strs) ^
                    "\n\n" ^
                    "\t.data\n" ^
                    "\t.align 0\n"^
                    (String.concat "" (List.map vaR8decl data)) ^
                    "\n"

        let print_simple_statement (s: sstmt) : unit = 
            print_endline (res_to_str (simple_compile_stmt s))

    end

module RIntList = RevList(struct type element = int end)
let (<@) a b = RIntList.app_list a b

let mk_revapp_test (r: RIntList.rlist) (expt: int list) (name: string) = 
    mk_verbose_expect_test (fun () -> RIntList.to_list r)
        expt
        (fun l -> "[ " ^ (List.fold_left (fun a i -> a ^ (string_of_int i) ^"; ") "" l) ^ "]")
        name
let revapp_test1 = mk_revapp_test 
    (RIntList.empty <@ [1;2] <@ [3;4])
    [1;2;3;4]
    "Reversed list test 1"

let revapp_test2 = 
    let t = RIntList.rev_list [1;2] in
        mk_revapp_test
            (t<@ [3;4])
            [1;2;3;4]
            "Reversed list test 2"


let _ = SAst.print_simple_statement SAst.simple_op

let _ = run_test_set [ revapp_test1;
                       revapp_test2;] "Reverse List Tests";;
let _ = run_test_set [ test_stub; ] "Compiler Tests";; 

(* TODO: IMPLEMENT TESTS *)
