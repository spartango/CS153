# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc
 
all: clean compile test

compile:
	$(COMPILER) -c ast.ml
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c eval.ml
	$(COMPILER) -c word32.ml
	$(COMPILER) -c mips.ml
	$(COMPILER) -c utility.ml
	$(COMPILER) -c compile.ml
	$(COMPILER) -c cish.ml
	$(COMPILER) -c pretty_print.ml
	$(COMPILER) -c test_framework.ml
	$(COMPILER) -c compile_tests.ml
	$(COMPILER) -o ps3 utility.cmo ast.cmo parse.cmo lex.cmo eval.cmo word32.cmo mips.cmo compile.cmo cish.cmo
	$(COMPILER) -o tests utility.cmo ast.cmo parse.cmo lex.cmo eval.cmo word32.cmo mips.cmo compile.cmo pretty_print.cmo test_framework.cmo compile_tests.cmo

test: compile 
	./tests

compile_tests: test
	./compile_canned.sh

run_tests: compile_tests
	./run_tests.sh

benchmark: compile_tests
	./benchmark.sh

toplevel: compile
	ocamlmktop -o toplvl utility.cmo ast.cmo parse.cmo lex.cmo eval.cmo word32.cmo mips.cmo compile.cmo pretty_print.cmo test_framework.cmo

xcompile:
	./xcompile_js.sh

run_js_tests: xcompile
	./run_js_tests.sh

compare_results: compile_tests xcompile
	./compare_results.sh

clean:
	rm -rf *.cmo *.cmi ps3 tests parse.ml parse.mli lex.ml compiled_tests toplvl compiled_js
