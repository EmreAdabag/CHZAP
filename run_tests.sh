ocamlbuild test_parser.native            
ocamlbuild test_semantic.native

rm -rfv log/* 
for file in `find . -path "*/tests/*" -name "*.chzap"` ; do
    f="$(basename -- $file)"
    echo "running $f"
    filename="${f%.*}"
    parser_output=$(cat $file | ./test_parser.native  2>&1)
    ##write to individual files
    echo "">>log/$filename.log  
    echo "---------parser_output---------">>log/$filename.log
    echo "$parser_output">>log/$filename.log
    
    semantic_output=$(cat $file | ./test_semantic.native  2>&1)
    echo "">>log/$filename.log
    echo "---------semantic_output---------">>log/$filename.log
    echo "$semantic_output">>log/$filename.log
    ##write to all
    echo "---------$filename---------">>log/all.log
    echo "parser_output:">>log/all.log
    echo "$parser_output">>log/all.log
    echo "">>log/all.log
    echo "semantic_output:">>log/all.log
    echo "$semantic_output">>log/all.log
done
