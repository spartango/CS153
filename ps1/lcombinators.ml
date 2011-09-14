(*********************************************************************)
(* This structure defines a set of generic combinators for building  *)
(* parsers -- routines that pattern match a list of values and build *)
(* results.  This generalizes the recognizer in two critical ways:   *)
(* First, it returns a computed value instead of just matching       *)
(* against the input stream.  Second, it can operate over arbitrary  *)
(* lists, instead of just lists of characters.  This will make it    *)
(* much easier to break the process of parsing into modular, re-     *)
(* usable components.                                                *)
(*********************************************************************)
module GenericParsing = 
struct
  type 'a llist = Nil | Cons of 'a * (('a llist) Lazy.t)

  let singleton x = Cons (x,lazy Nil)

  let rec lappend (x:'a llist) (y:'a llist Lazy.t) : 'a llist = 
    match x with 
      | Nil -> Lazy.force y
      | Cons (h,t) -> Cons (h, lazy (lappend (Lazy.force t) y))

  let rec lmap (f:'a -> 'b) (xs:'a llist) : 'b llist = 
    match xs with 
      | Nil -> Nil
      | Cons (h,t) -> Cons (f h, lazy (lmap f (Lazy.force t)))

  let rec lflatten (xs:'a llist llist) : 'a llist = 
    match xs with 
      | Nil -> Nil
      | Cons (h,t) -> lappend h (lazy (lflatten (Lazy.force t)))

  (* if [p] is a ('c,'a) parser, then informally, it consume lists of 'c 
   * values & produces an 'a value.  In practice, p applied to some list cs 
   * will return [] if the parser fails to match the list, and [(a1,cs1),...
   * (an,csn)] if it succeeds, where each ai is an 'a value, and each csi is 
   * a suffix of cs that was not consumed by the parser. *)
  type ('c,'a) parser = 'c list -> ('a * ('c list)) llist

  (* the [always x] parser consumes no input and always returns x *)
  let always(x:'a) : ('c,'a) parser = fun cs -> singleton (x,cs)

  (* the [never] parser fails, regardless of the input *)
  let never : ('c,'a) parser = fun cs -> Nil

  (* the [eof] parser succeeds only when we are at the end of the file *)
  let eof : ('c,unit) parser = 
      fun cs -> match cs with 
                   [] -> singleton ((),[])
                 | _ -> Nil
  (* the [alt(p1,p2)] parser first tries to parse with p1 and if it fails,
   * tries p2. *)                                                              
  let alt((p1:('c,'a) parser), (p2:('c,'a) parser)) : ('c,'a) parser = 
      fun cs -> lappend (p1 cs) (lazy (p2 cs))

  (* convert SML foldr to OCaml fold_right *)
  let foldr fn b lst = List.fold_right (fun x y -> fn (x,y)) lst b
  (* the parser [alts[p1,p2,...,pn]] generalizes alt to a list of parsers *)
  let alts(ps:('c,'a) parser list) : ('c,'a) parser = foldr alt never ps

  (* the parser [satisfy pred] reads a token [h] from the input list -- if
   * [pred h] returns true, then the parser succeeds, consuming and return
   * [h] as the result.  If [pred h] returns false, then the parser fails. *)
  let satisfy(pred:'c -> bool) : ('c,'c) parser = 
      fun cs -> (match cs with 
                    [] -> Nil
                  | h::t -> if pred h then singleton (h,t) else Nil)

  (* the parser [cat(p1,p2)] succeeds if the input list can be split into a
   * a prefix that matches p1, followed by a suffix that matches p2. *)
  let seq((p1:('c,'a) parser), (p2:('c,'b) parser)) : ('c,'a * 'b) parser = 
      fun cs -> 
        lflatten (lmap (fun (v1,cs1) -> 
                          lmap (fun (v2,cs2) -> 
                                  ((v1,v2),cs2)) (p2 cs1)) (p1 cs))
  (* if the parser [p] succeeds and returns value [v], then [map f p] succeeds
   * and returns [f v].  So map provides a way to transform a parser that 
   * return values of one type to a parser that returns values of another 
   * type. *)
  let map(f:'a->'b)(p:('c,'a) parser) : ('c,'b) parser = 
      fun cs -> lmap (fun (v,cs) -> (f v,cs)) (p cs)

  (* the parser [cons(p1,p2)] is like [seq(p1,p2)] but adds the value returned
   * by [p1] to the front of the list returned by [p2]. *)
  let cons((p1:('c,'a) parser), (p2:('c,'a list) parser)) : ('c,'a list) parser = 
      map (fun (elt,lst) -> elt::lst) (seq (p1,p2))

  (* the parser [list[p1,p2,...,pn]] is an abbreviation for 
   * [cons(p1,(cons(p2,...,(cons pn,always nil))))]. *)
  let list(ps:('c,'a) parser list) : ('c,'a list) parser = foldr cons (always []) ps

  (* the parser [star p] matches zero or more occurrences of strings that p 
   * matches, returning the result as a list. *)
  let rec star(p:('c,'a) parser) : ('c,'a list) parser = 
   fun cs -> (alt(cons(p,star p),always [])) cs

  (* [plus p] matches one or more occurrences of strings that p matches, 
   * returning the result as a list. *)
  let plus(p:('c,'a) parser) : ('c,'a list) parser = cons(p,star p)

  (* [opt p] matches an optional [p] *)
  let opt(p:('c,'a) parser) : ('c,'a option) parser = 
      alt (map (fun x -> Some x) p, always None)

  (* [unit p] matches when [p] does, but maps the result to the dummy value () *)
  let unit(p:('c,'a) parser) : ('c,unit) parser = map (fun _ -> ()) p

end (* struct Parsing *)


(*********************************************************************)
(* This structure uses the GenericParsing routines to build some     *)
(* useful parsing routines for lists of characters                   *)
(*********************************************************************)
module CharParsing = 
struct
  (* inherit all of the definitions from GenericParsing *)
  open GenericParsing
  (* get explode/implode library functions *)
  open Explode

  (* matches the character c and returns the character c *)
  let c(c:char) : (char,char) parser = satisfy (fun h -> c = h)

  (* matches any character x not equal to c and returns x *)
  let notC(c:char) : (char,char) parser = satisfy (fun h -> c <> h)

  (* this parser matches any character and returns it *)
  let anyC : (char,char) parser = satisfy (fun _ -> true)

  (* [lc_alpha] matches any lower-case letter and returns it*)
  let lc_alpha : (char,char) parser = 
      alts (List.map c (explode "abcdefghijklmnopqrstuvwxyz"))

  (* [uc_alpha] matches any upper-case letter and returns it*)
  let uc_alpha : (char,char) parser = 
      alts (List.map c (explode "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

  (* [alpha] matches any lower-case or upper-case letter and returns it*)
  let alpha : (char,char) parser = alt (lc_alpha, uc_alpha)

  (* [dig] matches any digit, returning the result as a character *)
  let dig : (char,char) parser = alts (List.map c (explode "0123456789"))

  (* [digit] is similar to [dig], but maps the result to the 
   * corresponding integer *)
  let digit : (char,int) parser = map (fun c -> Char.code c - Char.code '0') dig

  let underscore : (char,char) parser = c '_'

  (* [identifier] matches strings that start with an alphabetic character or
   * underscore, followed by zero or more alphabetic characters, digits, or
   * underscores.  The resulting string is returned as the result. *)
  let identifier : (char,string) parser = 
    map implode (cons (alts [alpha; underscore], 
                       star (alts [alpha; dig; underscore])))

  let foldl fn b = fun lst -> List.fold_left (fun y x -> fn (x,y)) b lst
  (* [integer] matches strings of digits and returns the corresponding integer
   * value *)
  let integer : (char,int) parser = 
      map (foldl (fun (d,a) -> a*10 + d) 0) (plus digit)

  (* [white] matches spaces, tabs, newlines, and carriage returns *)
  let white : (char,unit) parser = 
      unit(plus (alts [c ' '; c '\t'; c '\n'; c '\r']))

  (* [comment] matches ML-style comments that begin with (* and end with *).  
   * However, note that unlike ML, this won't handle nested comments!  Can
   * you transform it so it does? *)
  let comment : (char,unit) parser = 
      unit(seq ((seq(c '(', c '*')), 
             seq(star (alts [unit(notC '*'); unit(seq(c '*',notC ')'))]),
                 (seq(c '*', c ')')))))

  (* [whitespace] matches zero or more comments, spaces, tabs, etc. *)
  let whitespace = unit(star (alt (comment,white)))

  (* [str "foo"] matches the string "foo" *)
  let str(s:string) : (char,unit) parser = unit(list(List.map c (explode s)))
end

