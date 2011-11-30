# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc
 
all: clean cish cfg

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

cfg: mips cish
	$(COMPILER) -c utility.ml
	$(COMPILER) -c cish_ast.ml
	$(COMPILER) -c cfg_ast.ml
	$(COMPILER) -c io_types.ml
	$(COMPILER) -c pretty_print.ml
	$(COMPILER) -c cfg_gen.ml
	$(COMPILER) -c i_graph.ml
	$(COMPILER) -c cfg.ml
	$(COMPILER) -o ps7_cfg pretty_print.cmo utility.cmo cish_ast.cmo cish_lex.cmo cish_parse.cmo word32.cmo mips.cmo cfg_ast.cmo io_types.cmo cfg_gen.cmo i_graph.cmo cfg.cmo

test_files:
	$(COMPILER) -c pretty_print.ml
	$(COMPILER) -c test_framework.ml
	$(COMPILER) -c test_ioblocks.ml	

cfg_gen_tests: cfg cish mips test_files
	$(COMPILER) -c cfg_gen_tests.ml
	$(COMPILER) -o cfg_gen_tests utility.cmo cish_ast.cmo cish_lex.cmo cish_parse.cmo word32.cmo mips.cmo cfg_ast.cmo io_types.cmo cfg_gen.cmo pretty_print.cmo test_framework.cmo test_ioblocks.cmo cfg_gen_tests.cmo
	./cfg_gen_tests

i_graph_tests: cfg cish mips test_files
	$(COMPILER) -c i_graph_tests.ml
	$(COMPILER) -o i_graph_tests utility.cmo pretty_print.cmo cish_ast.cmo cish_lex.cmo cish_parse.cmo word32.cmo mips.cmo cfg_ast.cmo io_types.cmo cfg_gen.cmo i_graph.cmo pretty_print.cmo test_framework.cmo test_ioblocks.cmo i_graph_tests.cmo
	./i_graph_tests


io_tests: cfg cish mips test_files
	$(COMPILER) -c io_tests.ml
	$(COMPILER) -o io_tests utility.cmo cish_ast.cmo cish_lex.cmo cish_parse.cmo word32.cmo mips.cmo cfg_ast.cmo io_types.cmo cfg_gen.cmo pretty_print.cmo test_framework.cmo io_tests.cmo
	./io_tests

test: cfg_gen_tests i_graph_tests io_tests

toplevel: cfg
	ocamlmktop -o toplevel utility.cmo cish_ast.cmo cish_lex.cmo cish_parse.cmo word32.cmo mips.cmo cfg_ast.cmo io_types.cmo cfg_gen.cmo pretty_print.cmo test_framework.cmo cfg_gen_tests.cmo

clean:
	rm -rf *.cmo *.cmi ps7_cfg cfg_gen_tests i_graph_tests io_tests cish_parse.ml cish_parse.mli cish_lex.ml toplevel
