.PHONY : all
all : chzap.native lib.o

chzap.native:
	opam exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis chzap.native
	gcc -c lib.c

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff
	rm -rf \
	*.cmi *.cmo chzapparse.ml chzapparse.mli chzapparse.output scanner.ml \
        repl.out repl *.out __pycache__ _build chzap.native \
	*.ll *.exe *.s *.o testall.log
