#!/bin/bash

llvm=$1
file=$(echo $llvm | cut -f1 -d.)
if [ -z $2 ]; then
   :
else
	file2=$2
fi
./bugsy.native $llvm > "$file.ll"
llc "$file.ll" -o "$file.s"

if [ -z $file2 ]; then

	gcc "$file.s" -no-pie
else
	gcc "$file.s" -no-pie -o $file2
fi

rm "$file.ll" "$file.s"
