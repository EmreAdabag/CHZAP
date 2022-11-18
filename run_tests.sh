ocamlbuild test1.native                      
for file in `find . -path "*/tests/*" -name "*.chzap"` ; do
    echo $file
    cat $file | ./test1.native
done