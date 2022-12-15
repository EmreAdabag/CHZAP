.PHONY : all
all : test_parser.native test_semantic.native test_irgen.native

test_parser :
	ocamlbuild test_parser.native

test_semantic :
	ocamlbuild test_semantic.native

irgen:
	ocamlbuild -pkgs llvm chzap.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff
	rm -rf \
	*.cmi *.cmo chzapparse.ml chzapparse.mli chzapparse.output scanner.ml \
        repl.out repl *.out __pycache__ _build chzap.native \
	*.ll *.exe *.s *.o testall.log
