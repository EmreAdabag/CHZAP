## Make the parser and scanner
To make both the parser and the semantic checker run
```
make all
```
To only compile the parser run
```
make test_parser.native
```
To only compile the semantic checker run
```
make test_semantic.native
```
## Test cases
```
bash run_tests.sh
```
The output of each individual test would be under the `log` directory. The cumulative output would be in the file `log/all.log`

## IRGen
Compile: 
```
ocamlbuild -pkgs llvm chzap.native
```
Run: 
```
./chzap.native [-a|-s|-l] [file.chzap]
```
Each flag represent a different phase of the compiler.
`-a` produces the abstract syntax tree, which represent Syntax Analysis
`-s` Is Semantic Analysis
`-i` is the Intermediate Code Generation
# Features we implemented
1. strong static type
2. static scoping
3. Strict evalutaion semantics
4. higher order functions
5. First Class function
6. anonymous functions
7. Workflow: If,else, for, while, break, continue
8. Function decleration and function calling
