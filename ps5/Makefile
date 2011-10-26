# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc
 
all: clean cish scish mlish

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
	$(COMPILER) -o ps5_cish cish_ast.cmo cish_parse.cmo cish_lex.cmo cish_eval.cmo word32.cmo mips.cmo cish_compile.cmo cish.cmo

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
	$(COMPILER) -o ps5_scish cish_ast.cmo scish_ast.cmo scish_parse.cmo scish_lex.cmo scish_eval.cmo word32.cmo mips.cmo scish_compile.cmo scish.cmo

mlish: 
	$(COMPILER) -c mlish_ast.ml
	$(COMPILER) -c scish_ast.ml
	$(COMPILER) -c scish_eval.ml
	ocamlyacc ml_parse.mly
	$(COMPILER) -c ml_parse.mli
	$(COMPILER) -c ml_parse.ml
	ocamllex ml_lex.mll
	$(COMPILER) -c ml_lex.ml
	$(COMPILER) -c mlish_type_check.ml
	$(COMPILER) -c mlish_compile.ml
	$(COMPILER) -c mlish.ml
	$(COMPILER) -o ps5_mlish mlish_ast.cmo scish_ast.cmo scish_eval.cmo ml_parse.cmo ml_lex.cmo mlish_type_check.cmo mlish_compile.cmo mlish.cmo

clean:
	-rm *.cmo *.cmi ps5_cish ps5_scish ps5_mlish cish_parse.ml cish_parse.mli cish_lex.ml scish_parse.ml scish_parse.mli scish_lex.ml ml_parse.ml ml_parse.mli ml_lex.ml
