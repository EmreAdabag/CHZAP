### Build the MicroC compiler

```
ocamlbuild -pkgs llvm microc.native
```

### Run the MicroC compiler and generate llvm code
```
./microc.native -l example.mc > example.out
```

### Run the llvm code
```
lli example.out
```

### Compiler files
-  `ast.ml`: abstract syntax tree (AST) definition
-  `scanner.mll`: scanner
-  `microcparse.mly`: parser
-  `sast.ml`: definition of the semantically-checked AST
-  `semant.ml`: semantic checking
-  `irgen.ml`: LLVM IR code generator

### Other files

- `test1.ml`: the file to test the scanner and parser
- `test2.ml`: the file to test the semantic checker
- `microc.ml`: top-level file to test and run microc compiler
- `example.mc`: a sample microc source code
- `example.out`: a sample compiled code of example.mc


# CHZAP  READ ME Starts here 

to build the tester for the parser run the following command 

```
ocamlbuild test1.native
```

Then to test the program on the `tests/parser_exprs.chzap`, run 
```
cat tests/parser_exprs.chzap | ./test1.native
```