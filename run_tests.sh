echo ""
echo "Running Tests..."

echo ""
echo "Clearing:"

rm -rfv log/*
mkdir log/_build

echo ""
echo "Building:"
# ocamlbuild test_parser.native            
# ocamlbuild test_semantic.native
ocamlbuild -pkgs llvm chzap.native
gcc -c lib.c -o log/_build/lib.o

echo ""
echo "Testing:"

cnt=0
pass=0
fail=0

for file in `find . -path "*/tests/*" -name "*.chzap"` ; do
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

    ir_output=$(cat $file | ./chzap.native -l  2>&1)
    echo -e "$ir_output" > log/$filename.ll
    echo "">>log/$filename.log
    echo "---------ir_output---------">>log/$filename.log
    echo "$ir_output">>log/$filename.log

    program_output=$(
        llc log/$filename.ll -o log/_build/$filename.s  2>&1 && 
        gcc -c log/_build/$filename.s -o log/_build/$filename.o  2>&1 &&
        gcc log/_build/$filename.o log/_build/lib.o -o log/_build/$filename  2>&1 &&
        ./log/_build/$filename  2>&1
    )
    code=$?
    echo "">>log/$filename.log
    echo "---------program_output---------">>log/$filename.log
    echo "$program_output">>log/$filename.log

    ##catch err
    [[ $filename =~ ^error* ]]
    exp=$?
    if [[ $exp == $code ]]
    then
        fail=$((fail+1))
        echo "failed"
    else
        pass=$((pass+1))
        echo "passed"
    fi

    ##write to all
    echo "---------$filename---------">>log/all.log
    echo "parser_output:">>log/all.log
    echo "$parser_output">>log/all.log
    echo "">>log/all.log
    echo "semantic_output:">>log/all.log
    echo "$semantic_output">>log/all.log
    echo "">>log/all.log
    echo "ir_output:">>log/all.log
    echo "$ir_output">>log/all.log
    echo "">>log/all.log
    echo "program_output:">>log/all.log
    echo "$program_output">>log/all.log

    cnt=$((cnt+1))
done

echo ""
echo "Summary: $cnt tests conducted: $pass passed, $fail failed"
