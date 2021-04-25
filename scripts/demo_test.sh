#!/bin/bash
./bugsy.native $1.bug > "$1.ll"
llc "$1.ll" -o "$1.s"
echo "hi"
# gcc "src/builtins.c" -c -o "_build/builtins.o"
g++ "$1.s" "_build/builtins.o" -no-pie -lglut -lGL -lGLU -lGLEW -o $1
rm "$1.ll" "$1.s"

echo "$1 compiled"
