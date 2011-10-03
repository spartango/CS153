open Mips
open Ast
open Eval

(* Code Crawling Utilities *)

exception LabelNotFound

(* Finds a label, returning all the instructions that follow it *)
let rec find_label (target : string) (insts : inst list) : inst list = 
	match insts with
	| []                -> []
	| Label(name)::rest -> if (name = target) then rest else (find_label target rest)
	| _::rest           -> (find_label target rest)
;; 

(* Functions that optimize *)

(* Jump threading: eliminating jump->jump behavior *)
let thread_jumps (insts : inst list) : inst list =

	(* Follows jumps until the end, returning the last one *)
	let rec inspect_jump (last_label : string) : string = 
		(* Find the target in all code *)
		let target_inst = (find_label last_label insts) in
		match target_inst with 
		| J(label_name)::_ -> (inspect_jump label_name)
		| _ 			   -> last_label
	in 
	
	(* For each instruction *)
	let rec thread_jumps_r (upper_segment : inst list)  (lower_segment : inst list) =
		(* Check if its a jump *)
		match lower_segment with 
		| []                  -> upper_segment
		| J(label_name)::rest -> thread_jumps_r (upper_segment @ [J(inspect_jump label_name)]) rest
	    | inst::rest          -> thread_jumps_r (upper_segment @ [inst]) rest
	in
	thread_jumps_r [] insts
;; 

(* Constant folding: cutting constant-constant ops out of the AST *)
let rec constant_fold  (statement : Ast.stmt) : Ast.stmt =
	
	(* Combine and *)
	let combine_and exp1 exp2 = 
		let (rexp1, _) = exp1 in
		let (rexp2, _) = exp2 in
		match (rexp1, rexp2) with
		| (Int(i1), Int(i2)) -> Int(bool2int ((i1 = 0) && (i2 = 0)))
		| _					 -> And(exp1, exp2)
	in 
	(* Combine or *)
	let combine_or exp1 exp2 = 
		let (rexp1, _) = exp1 in
		let (rexp2, _) = exp2 in
		match (rexp1, rexp2) with
		| (Int(i1), Int(i2)) -> Int(bool2int ((i1 = 0) || (i2 = 0)))
		| _					 -> Or(exp1, exp2)
	in 
	(* Combine binary operations if possible *)
	let combine_binop exp1 (op : binop) exp2 =
		let (rexp1, _) = exp1 in
		let (rexp2, _) = exp2 in
		match (rexp1, rexp2) with
		| (Int(i1), Int(i2)) ->
			(match op with 
			| Plus  -> Int(i1 +  i2)   
			| Minus -> Int(i1 -  i2)   
			| Times -> Int(i1 *  i2)   
			| Div   -> Int(i1 /  i2) 
			| Eq    -> Int(bool2int(i1 =  i2))
			| Neq   -> Int(bool2int(i1 != i2)) 
			| Lt    -> Int(bool2int(i1 <  i2))
			| Lte   -> Int(bool2int(i1 <= i2)) 
			| Gt    -> Int(bool2int(i1 >  i2))
			| Gte   -> Int(bool2int(i1 >= i2)))
		| (_, _) -> Binop(exp1, op, exp2)
	in
	(* Handle expressions *)
	let rec constant_fold_e (expression : Ast.exp) : Ast.exp =
		let (rexpr, position) = expression in
		let folded_expr = 
			match rexpr with 
			| Int(_)                -> rexpr
			| Var(_)                -> rexpr
			| Binop(exp1, op, exp2) -> (combine_binop (constant_fold_e exp1) op (constant_fold_e exp2))
			| Not(t_exp) 			-> Not(constant_fold_e t_exp)
			| And(exp1, exp2)		-> (combine_and (constant_fold_e exp1) (constant_fold_e exp2))
			| Or(exp1, exp2)		-> (combine_or  (constant_fold_e exp1) (constant_fold_e exp2))
			| Assign(v, t_exp)		-> Assign(v, (constant_fold_e t_exp))
		in 
		(folded_expr, position)
	in

	(* Break statements down *)
	let (rstatement, position) = statement in
	let folded_statement = 
		match rstatement with 
		| Seq(s1, s2)                    -> Seq((constant_fold s1), (constant_fold s2))
		| Exp(expr)                      -> Exp(constant_fold_e expr)
		| Return(expr)                   -> Return(constant_fold_e expr)
		| If(expr, then_s, else_s)       -> If((constant_fold_e expr), (constant_fold then_s), (constant_fold else_s))
		| While(expr, do_s)  	         -> While((constant_fold_e expr), (constant_fold do_s))
		| For(expr1, expr2, expr3, do_s) -> For((constant_fold_e expr1), (constant_fold_e expr2), (constant_fold_e expr3), (constant_fold do_s))
	in
	(folded_statement, position)
