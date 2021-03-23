#!/bin/bash

file=$1
../microc.native $file > "$file.ll"
llc "$file.ll" -o "$file.s"
gcc "$file.s" -no-pie
rm "$file.ll" "$file.s"
