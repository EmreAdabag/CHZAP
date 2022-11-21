.PHONY : all
all : test1.native test2.native

test1.native :
	ocamlbuild test1.native

test2.native :
	ocamlbuild test2.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff
	rm -rf \
	*.cmi *.cmo chzapparse.ml chzapparse.mli chzapparse.output scanner.ml \
        repl.out repl *.out __pycache__ _build chzap.native \
	*.ll *.exe *.s *.o testall.log