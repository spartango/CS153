# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc
 
all: clean mips

test: mips
	./tests

mips:
	$(COMPILER) -c binary_ops.ml
	$(COMPILER) -c mips_ast.ml
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c byte.ml
	$(COMPILER) -c mips_sim.ml
	$(COMPILER) -c mips.ml
	$(COMPILER) -c pretty_print.ml
	$(COMPILER) -c test_framework.ml
	$(COMPILER) -c mips_tests.ml
	$(COMPILER) -o ps0 binary_ops.cmo mips_ast.cmo parse.cmo lex.cmo byte.cmo mips_sim.cmo mips.cmo 
	$(COMPILER) -o tests binary_ops.cmo mips_ast.cmo byte.cmo mips_sim.cmo pretty_print.cmo test_framework.cmo mips_tests.cmo
	
clean:
	-rm *.cmo *.cmi ps0 tests parse.ml parse.mli lex.ml 
