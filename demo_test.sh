#!/bin/sh
filename=$(basename demo.mc)
./microc.native demo.mc > "demo.ll"
llc "demo.ll" -o "demo.s"
gcc "printbig.c" -c -o "printbig.o"
g++ "demo.s" "printbig.o" -lglut -lGL -lGLU -lGLEW -o test
rm "demo.ll" "demo.s"

done
echo "Done running tests"
