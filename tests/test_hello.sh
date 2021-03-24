#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

function red {
    printf "${RED}$@${NC}\n"
}

function green {
    printf "${GREEN}$@${NC}\n"
}

function yellow {
    printf "${YELLOW}$@${NC}\n"
}

echo "Starting tests"
for i in *.mc; do
    filename=$(basename $i .bug)
    ../bugsy.native $i > "$i.ll"
    llc "$i.ll" -o "$i.s"
    gcc "$i.s" -no-pie
    rm "$i.ll" "$i.s"

    ./a.out > "$i.run"
    diff "$i.run" "$filename.out"
    if [ $? -eq 0 ]
    then
	echo $(green "Test $i passed!")
    elif [ $? -eq 1 ]
    then
        echo $(red "Test $i failed!")
    fi
    rm "$i.run" a.out
done
echo "Done running tests"
