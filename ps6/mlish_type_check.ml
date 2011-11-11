open Mlish_ast

exception Invalid
exception TypeError
exception NotFound

(* Used whenever a type error is encountered *)
let type_error(s:string) = (print_string (s^"\n"); raise TypeError)

(* fold over two lists, outputting the union of the elements *)
let union = List.fold_right (fun x ys -> if List.memq x ys then ys else x::ys)

(* remove all of the elements from xs that are in ys *)
let diff xs ys = 
    let f = fun x r -> if List.memq x ys then r else x :: r in
    List.fold_right f xs []

(* A fresh guess *)
let guess() = Guess_t(ref None)

(* Lookup a tipe_scheme in our env *)
let rec lookup (env : (var * tipe_scheme) list) (v: var) : tipe_scheme =
  match env with
      (v', t)::tl -> if v' = v then t else lookup tl v
    | [] -> raise NotFound

(* extend our env with v * s *)
let extend (env : (var * tipe_scheme) list) (v: var) (s: tipe_scheme) =
  (v,s)::env

(* For creating fresht vars *)
let var_counter = ref 0
let new_int() = var_counter := !var_counter + 1; !var_counter
let freshvar() = "t" ^ (string_of_int (new_int ()))

(* Collapse chains of Guess's to just a single Guess *)
let rec collapse (t : tipe) : tipe =
  match t with
    | Guess_t ({contents = Some t1} as r) -> 
      let t2 = collapse t1 in
       r := Some t2 ; t2
    | _ -> t

(* Substitute all of the tvars in t with the tipe found in l *)
let rec substitute (l: (tvar * tipe) list) (t: tipe) =
  let t = collapse t in
  match t with
    | Fn_t (t1, t2) -> Fn_t (substitute l t1, substitute l t2)
    | Pair_t (t1, t2) -> Pair_t (substitute l t1, substitute l t2)
    | List_t t -> List_t (substitute l t)
    | Tvar_t t1 -> (try List.assoc t1 l with Not_found -> t)
    | _ -> t

(* refresh all tvars with fresh guesses *)
let instantiate (s : tipe_scheme) : tipe=
  let Forall(vs, t) = s in
  let vs_and_ts = List.map (fun a -> (a,guess())) vs in
    substitute vs_and_ts t

(* extract all guesses from t *)
let rec guesses_of_tipe (t : tipe) : (tipe option ref) list =
  let t = collapse t in
  match t with
    | Tvar_t _ | Int_t | Bool_t | Unit_t -> []
    | Fn_t (t1, t2) | Pair_t (t1, t2) -> union (guesses_of_tipe t1) (guesses_of_tipe t2)
    | List_t t -> guesses_of_tipe t
    | Guess_t r -> [r]

(* extract the guesses from the tipe of the given tipe_scheme *)
let guesses_of (s : tipe_scheme) : (tipe option ref) list = 
  let Forall (_, t) = s in guesses_of_tipe t

(* Replace all guesses in t with the corresponding tvars found in gs *)
let rec subst_guess (gs : (tipe option ref * tvar) list) (t : tipe) : tipe =
  let t = collapse t in
  match t with
    | Fn_t (t1, t2) -> Fn_t (subst_guess gs t1, subst_guess gs t2)
    | Guess_t r -> (try Tvar_t (List.assq r gs) with Not_found -> t)
    | List_t t1 -> List_t (subst_guess gs t1)
    | Pair_t (t1, t2) -> Pair_t (subst_guess gs t1, subst_guess gs t2)
    | _ -> t

(* Generalize all guesses in t that aren't found in env *)
let generalize (env : (var*tipe_scheme) list) (t: tipe) =
  let t_gs = guesses_of_tipe t in
  let env_list_gs = List.map (fun (x,s) -> guesses_of s) env in
  let env_gs = List.fold_left union [] env_list_gs in
  let diff = diff t_gs env_gs in
  let gs_vs = List.map (fun g -> (g, freshvar())) diff in
  let tc = subst_guess gs_vs t in
  Forall (List.map snd gs_vs, tc)

(* Check if a given Guess appears in the tipe *) 
let rec occurs (r : tipe option ref) (t : tipe) : bool =
  match t with
   | Guess_t r' -> r == r'
   | List_t t1 -> occurs r t1
   | Pair_t (t1, t2) -> occurs r t1 || occurs r t2
   | Fn_t (t1, t2) -> occurs r t1 || occurs r t2
   | _ -> false

(* Unify t1 and t2, raising a type error if they can't be equated *)
let rec unify (t1: tipe) (t2: tipe) : unit =
  if t1 == t2 then () else
    match collapse t1, collapse t2 with
     | Guess_t ({contents = None} as r), t
     | t, Guess_t ({contents = None} as r) ->
         if occurs r t then type_error "occurs fail" else
         r := Some t
     | Fn_t(t1a, t1b), Fn_t(t2a, t2b) -> unify t1a t2a ; unify t1b t2b 
     | Pair_t(t1a, t1b), Pair_t(t2a, t2b) -> unify t1a t2a ; unify t1b t2b
     | List_t t1, List_t t2 -> unify t1 t2
     | Tvar_t t, _ -> ()
     | _, Tvar_t t -> ()
     | Bool_t, Bool_t | Int_t, Int_t | Unit_t, Unit_t -> ()
     | _ -> 
         let _ = print_string ((t2s t1)^" and "^(t2s t2) ^"\n") in
         type_error "bad unify"
        

(* Typecheck an expression with the given env *)
let rec tc (env: (var*tipe_scheme) list) ((e,_) : exp) : tipe=
  
  (* Type-check a primapp and return the expected return type *)
  let tc_op t1 t2 p =
    unify t1 Int_t; unify t2 Int_t;
      match p with
        | Plus | Minus | Times | Div -> Int_t
        | Eq | Lt -> Bool_t
        | _ -> type_error "invalid dual-op"
  in
  match e with
    | Var x -> instantiate (lookup env x)
    | PrimApp (p, xs) -> 
        (match p, xs with 
           | Int _, [] -> Int_t
           | Bool _, [] -> Bool_t
           | Unit, [] -> Unit_t
           | Plus, [e1;e2] | Minus, [e1;e2] | Times, [e1;e2] 
           | Div, [e1;e2] | Eq, [e1;e2] | Lt, [e1;e2] ->
              tc_op (tc env e1) (tc env e2) p
           | Pair, [e1;e2] -> Pair_t (tc env e1, tc env e2)
           | Fst, [e1] -> 
               let t1 = tc env e1 in 
               let g1,g2 = (guess()), (guess()) in
                   unify t1 (Pair_t (g1, g2)); g1
           | Snd, [e1] ->
              let t1 = tc env e1 in 
               let g1 = guess() in
               let g2 = guess() in
                 unify t1 (Pair_t (g1, g2)); g2
           | Nil, [] -> List_t (guess())
           | Cons, [e1;e2] -> 
              let (t1,t2,t) = ((tc env e1), (tc env e2), guess()) in
                unify (List_t t1) t2; List_t t1
           | IsNil, [e1] -> 
               let g= guess() in 
               let t1 = tc env e1 in
                 unify t1 (List_t g); Bool_t
           | Hd, [e1] -> 
               let g= guess() in 
               let t1 = tc env e1 in
                 unify t1 (List_t g); g

           | Tl, [e1] ->
               let g= guess() in 
               let t1 = tc env e1 in
                 unify t1 (List_t g); List_t g 
               
           | _ -> type_error "invalid primapp specification")
    | Fn (v, e1) ->
        let g = guess() in
        Fn_t(g, tc (extend env v (Forall([],g))) e1)
    | App (e1, e2) -> 
        let (t1,t2,t) = ((tc env e1), (tc env e2), guess()) in
        unify t1 (Fn_t(t2,t)); t
    | If (e1, e2, e3) ->
        unify (tc env e1) Bool_t;
        let t2 = (tc env e2) in
        let t3 = (tc env e3) in
          unify t2 t3; t2
    | Let (x, e1, e2) ->
        let s = (generalize env (tc env e1)) in
        tc (extend env x s) e2

let type_check_exp (e:Mlish_ast.exp) : tipe = tc [] e
