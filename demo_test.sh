#!/bin/bash
./bugsy.native $1.bug > "$1.ll"
llc "$1.ll" -o "$1.s"
g++ "$1.s" "builtins.o" -no-pie -lglut -lGL -lGLU -lGLEW -o $1
rm "$1.ll" "$1.s"

echo "$1 compiled"
