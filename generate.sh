# $1 = filename
# $2 = architecture; e.g. arm64, x86_64

./chzap.native $1.chzap > $1.ll
llc -march=$2 $1.ll -o $1.s
gcc -c $1.s   
gcc $1.o lib.o -o $1