# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc
 
all: clean
	$(COMPILER) -c ast.ml
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c eval.ml
	$(COMPILER) -c word32.ml
	$(COMPILER) -c mips.ml
	$(COMPILER) -c compile.ml
	$(COMPILER) -c fish.ml
	$(COMPILER) -o ps3 ast.cmo parse.cmo lex.cmo eval.cmo  word32.cmo mips.cmo compile.cmo fish.cmo

clean:
	-rm *.cmo *.cmi ps3 parse.ml parse.mli lex.ml
