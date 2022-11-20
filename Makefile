.PHONY : all
all : chzap.native

chzap.native :
	ocamlbuild chzap.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff
	rm -rf \
	*.cmi *.cmo chzapparse.ml chzapparse.mli chzapparse.output scanner.ml \
        repl.out repl *.out __pycache__ _build chzap.native \
	*.ll *.exe *.s *.o testall.log

TARFILES = README.md Makefile \
	scanner.mll chzapparse.mly ast.ml sast.ml semant.ml \
	irgen.ml chzap.ml

zip :
	zip chzap $(TARFILES)