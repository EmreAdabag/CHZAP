# ocamlbuild test_parser.native            
# ocamlbuild test_semantic.native
ocamlbuild -pkgs llvm chzap.native

rm -rfv log/* 
for file in `find . -path "*/tests/test-function-*" -name "*.chzap"` ; do
    f="$(basename -- $file)"
    echo -n "running $f ... "
    filename="${f%.*}"
    parser_output=$(cat $file | ./chzap.native -a  2>&1)
    ##write to individual files
    echo "">>log/$filename.log  
    echo "---------parser_output---------">>log/$filename.log
    echo "$parser_output">>log/$filename.log
    
    semantic_output=$(cat $file | ./chzap.native -s  2>&1)
    echo "">>log/$filename.log
    echo "---------semantic_output---------">>log/$filename.log
    echo "$semantic_output">>log/$filename.log

    program_output=$(cat $file | ./chzap.native -l > log/$filename.ll && lli log/$filename.ll)
    code=$(echo $?)
    echo "">>log/$filename.log
    echo "---------program_output---------">>log/$filename.log
    echo "$program_output">>log/$filename.log

    ##catch err
    if [[ $f == "error*" ^ $code!=0 ]]
    then
        info_output="err"
        >&2 echo $info_output
    else
        info_output="yes"
        echo $info_output
    fi

    ##write to all
    echo "---------$filename---------">>log/all.log
    echo "parser_output:">>log/all.log
    echo "$parser_output">>log/all.log
    echo "">>log/all.log
    echo "semantic_output:">>log/all.log
    echo "$semantic_output">>log/all.log
    echo "">>log/all.log
    echo "program_output:">>log/all.log
    echo "$program_output">>log/all.log
done
