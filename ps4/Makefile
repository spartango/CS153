# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc
 
all: clean cish scish

mips:
	$(COMPILER) -c word32.ml
	$(COMPILER) -c mips.ml

cish: mips
	$(COMPILER) -c cish_ast.ml
	ocamlyacc cish_parse.mly
	$(COMPILER) -c cish_parse.mli
	$(COMPILER) -c cish_parse.ml
	ocamllex cish_lex.mll
	$(COMPILER) -c cish_lex.ml
	$(COMPILER) -c cish_eval.ml
	$(COMPILER) -c cish_compile.ml
	$(COMPILER) -c cish.ml
	$(COMPILER) -o ps4_cish cish_ast.cmo cish_parse.cmo cish_lex.cmo cish_eval.cmo word32.cmo mips.cmo cish_compile.cmo cish.cmo

scish: mips
	$(COMPILER) -c cish_ast.ml
	$(COMPILER) -c scish_ast.ml
	ocamlyacc scish_parse.mly
	$(COMPILER) -c scish_parse.mli
	$(COMPILER) -c scish_parse.ml
	ocamllex scish_lex.mll
	$(COMPILER) -c scish_lex.ml
	$(COMPILER) -c scish_eval.ml
	$(COMPILER) -c scish_compile.ml
	$(COMPILER) -c scish.ml
	$(COMPILER) -o ps4_scish cish_ast.cmo scish_ast.cmo scish_parse.cmo scish_lex.cmo scish_eval.cmo word32.cmo mips.cmo scish_compile.cmo scish.cmo

clean:
	-rm *.cmo *.cmi ps4_cish ps4_scish cish_parse.ml cish_parse.mli cish_lex.ml scish_parse.ml scish_parse.mli scish_lex.ml
