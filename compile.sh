#!/bin/bash

llvm=$1
file=$(echo $llvm | cut -f1 -d.)
if [ $2 -ne 0 ]; then
	file2=$2
fi
./bugsy.native $llvm > "$file.ll"
llc "$file.ll" -o "$file.s"

if [ -z ${file2+x} ]; then
	gcc "$file.s" -no-pie -o $file2
else
	gcc "$file.s" -no-pie
fi

rm "$file.ll" "$file.s"
