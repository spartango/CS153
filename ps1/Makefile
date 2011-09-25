# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc
 
all: clean comb testall

utility:
	$(COMPILER) -c explode.ml

com:
	$(COMPILER) -c ast.ml
	$(COMPILER) -c eval.ml

testf:
	$(COMPILER) -c pretty_print.ml
	$(COMPILER) -c test_framework.ml

comb: utility com
	$(COMPILER) -c lcombinators.ml
	$(COMPILER) -c comblexer.ml
	$(COMPILER) -c combparser.ml
	$(COMPILER) -c fishcomb.ml
	$(COMPILER) -o ps1comb ast.cmo eval.cmo explode.cmo lcombinators.cmo comblexer.cmo combparser.cmo fishcomb.cmo

combtest: testf comb
	$(COMPILER) -c fishcomb_test.ml
	$(COMPILER) -o testcomb pretty_print.cmo test_framework.cmo ast.cmo eval.cmo explode.cmo lcombinators.cmo comblexer.cmo combparser.cmo fishcomb_test.cmo
	
yacc: com
	ocamlyacc -v parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c fishyacc_helpers.ml
	$(COMPILER) -c fishyacc.ml
	$(COMPILER) -o ps1yacc ast.cmo parse.cmo lex.cmo eval.cmo fishyacc_helpers.cmo fishyacc.cmo

yacctest: testf yacc
	$(COMPILER) -c fishyacc_test.ml
	$(COMPILER) -o testyacc pretty_print.cmo test_framework.cmo ast.cmo parse.cmo lex.cmo eval.cmo fishyacc_helpers.cmo fishyacc_test.cmo 

combtoplevel: comb
	ocamlmktop -o toplvl explode.cmo lcombinators.cmo comblexer.cmo ast.cmo combparser.cmo 

runcombtest: combtest
	./testcomb

runyacctest: yacctest
	./testyacc
	
testall: combtest runcombtest
	
clean:
	-rm *.cmo *.cmi toplvl ps1comb ps1yacc testyacc testcomb parse.ml parse.mli lex.ml
