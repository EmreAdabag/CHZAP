.PHONY : all
all : test_parser.native test_semantic.native

test_parser.native :
	ocamlbuild test_parser.native

test_semantic.native :
	ocamlbuild test_semantic.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff
	rm -rf \
	*.cmi *.cmo chzapparse.ml chzapparse.mli chzapparse.output scanner.ml \
        repl.out repl *.out __pycache__ _build chzap.native \
	*.ll *.exe *.s *.o testall.log
