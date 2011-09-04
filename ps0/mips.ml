open Mips_ast
open Mips_sim

let inst2str i =
  match i with
    Add (r1,r2,r3) -> "add "^(reg2str r1)^", "^(reg2str r2)^", "^(reg2str r3)^"\n"
  | Beq (r1,r2,imm) -> "beq "^(reg2str r1)^", "^(reg2str r2)^", "^(Int32.to_string imm)^"\n"
  | Jr (r) -> "jr "^(reg2str r)^"\n"
  | Jal (imm) -> "jal "^(Int32.to_string imm)^"\n"
  | Li (r,imm) -> "li "^(reg2str r)^", "^(Int32.to_string imm)^"\n"
  | Lui (r,imm) -> "lui "^(reg2str r)^", "^(Int32.to_string imm)^"\n"
  | Ori (r1,r2,imm) -> "ori "^(reg2str r1)^", "^(reg2str r2)^", "^(Int32.to_string imm)^"\n"
  | Lw (r1,r2,imm) -> "lw "^(reg2str r1)^", "^(Int32.to_string imm)^"("^(reg2str r2)^")\n"
  | Sw (r1,r2,imm) -> "sw "^(reg2str r1)^", "^(Int32.to_string imm)^"("^(reg2str r2)^")\n"

let rec prog2str p =
  match p with
    [] -> "\n"
  | hd::tl -> (inst2str hd)^(prog2str tl)

let parse_file() =
  let argv = Sys.argv in
  let _ =
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Parse.program Lex.lexer (Lexing.from_channel ch)

let parse_stdin() =
  Parse.program Lex.lexer (Lexing.from_channel stdin)

(* Expect 1 command line argument, the file to interpret
 * usage: ps0 [file-to-interpret] *)
let _ =
  let prog = parse_file() in
  let state' = interp (assem prog) in
  let _ = print_string ("Register File\n"^(string_of_rf state'.r)) in
  let _ = print_string ("Memory\n"^(string_of_mem state'.m)) in
  print_string ("PC = "^(Int32.to_string state'.pc)^"\n")
