ocamlbuild test1.native            
ocamlbuild test2.native               
for file in `find . -path "*/tests/*" -name "*.chzap"` ; do
    echo $file
    cat $file | ./test1.native
    cat $file | ./test2.native
done