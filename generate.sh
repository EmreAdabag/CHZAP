# $1 = filename
# $2 = architecture; e.g. arm64, x86_64

f="$(basename -- $1)"
filename="${f%.*}"
echo $filename

rm -rfv log/_build
mkdir log/_build

ocamlbuild -pkgs llvm chzap.native
./chzap.native $1 > log/$filename.ll
llc -march=$2 log/$filename.ll -o log/_build/$filename.s
gcc -c lib.c -o log/_build/lib.o
gcc -c log/_build/$filename.s -o log/_build/$filename.o
gcc log/_build/$filename.o log/_build/lib.o -o $filename