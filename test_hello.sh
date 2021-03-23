#!/bin/sh
echo "Starting tests"
cd hello_tests
for i in *.mc; do
    echo "$i"
    #../microc.native $i > "$i.ll"
    #llc "$i.ll" -o "$i.s"
    #gcc "$i.s" -no-pie
    ../compile.sh $i
    echo "Made it"
    ls
    ./a.out > "$i.run"
    diff "$i.run" "$i.out"
    rm "$i.ll" "$i.s" "$i.run"
done
echo "Done running tests"