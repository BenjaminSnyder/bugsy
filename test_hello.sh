#!/bin/sh
echo "Starting tests"
cd hello_tests
for i in *.mc; do
    filename=$(basename $i .mc)
    ../microc.native $i > "$i.ll"
    llc "$i.ll" -o "$i.s"
    gcc "$i.s" -no-pie
    rm "$i.ll" "$i.s"

    ./a.out > "$i.run"
    diff "$i.run" "$filename.out"
    if [ $? -eq 0 ]
    then
        echo "Test $i passed!"
    elif [ $? -eq 1 ]
    then
        echo "Test $i failed!"
    fi
    rm "$i.run" a.out
done
echo "Done running tests"