#!/bin/bash
./bugsy.native demo.mc > "demo.ll"
llc "demo.ll" -o "demo.s"
gcc "builtins.c" -c -o "builtins.o"
g++ "demo.s" "builtins.o" -lglut -lGL -lGLU -lGLEW -o demo
rm "demo.ll" "demo.s"

echo "Demo compiled"
