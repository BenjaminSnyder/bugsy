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

# Regression testing script for bugsy
# Step through a list of files
#  Compile, run, and check the output of each expected-to-work test
#  Compile and check the error of each expected-to-fail test

# Path to the LLVM interpreter
LLI="lli"
#LLI="/usr/local/opt/llvm/bin/lli"

# Path to the LLVM compiler
LLC="llc"

# Path to the C compiler
CC="cc"

# Path to the bugsy compiler.  Usually "./bugsy.native"
# Try "_build/bugsy.native" if ocamlbuild was unable to create a symbolic link.
BUGSY="./bugsy.native"
#BUGSY="_build/microc.native"

# Set time limit for all operations
ulimit -t 30

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

keep=0

Usage() {
    echo "Usage: testall.sh [options] [.bug files]"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
	#echo "FAILED"
    echo $(red "FAILED")
	error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

# RunFail <args>
# Report the command, run it, and expect an error
RunFail() {
    echo $* 1>&2
    eval $* && {
	SignalError "failed: $* did not report an error"
	return 1
    }
    return 0
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.bug//'`
    reffile=`echo $1 | sed 's/.bug$//' | sed 's/test\/tests//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`../golden_set"
    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    if [ ! -d tmp ]; then
        mkdir tmp
    fi
    generatedfiles=""

    generatedfiles="$generatedfiles tmp/${basename}.ll tmp/${basename}.s tmp/${basename}.exe tmp/${basename}.out" &&
    Run "$BUGSY" "$1" ">" "tmp/${basename}.ll" &&
    Run "$LLC" "-relocation-model=pic" "tmp/${basename}.ll" ">" "tmp/${basename}.s" &&
    #Run "$CC" "-o" "${basename}.exe" "${basename}.s" "printbig.o" &&
    Run "g++" "tmp/${basename}.s" "_build/builtins.o" "-no-pie" "-lglut" "-lGL" "-lGLU" "-lGLEW" "-o" "tmp/${basename}.exe" &&
    Run "DEBUG=1 ./tmp/${basename}.exe" > "tmp/${basename}.out" &&
    Compare "test/golden_set/${basename}.out" "tmp/${reffile}.out" "tmp/${basename}.diff"

    # Report the status and clean up the generated files
    if [ $error -eq 0 ] ; then
	    if [ $keep -eq 0 ] ; then
	      rm -f $generatedfiles
      fi
	#echo "OK"
    echo $(green "OK")
    #echo "Hi"

	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

CheckFail() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.bug//'`
    reffile=`echo $1 | sed 's/.bug$//' | sed 's/test\/tests//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`../fails"

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles tmp/${basename}.err tmp/${basename}.diff" &&
    RunFail "$BUGSY" "<" $1 "2>" "tmp/${basename}.err" ">>" "$globallog" &&
    Compare "test/fails/${basename}.err" "tmp/${basename}.err" "tmp/${basename}.diff"

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	fi
	#echo "OK"
    echo $(green "OK")
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

while getopts kdpsh c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
	h) # Help
	    Usage
	    ;;
    esac
done

shift `expr $OPTIND - 1`

LLIFail() {
  echo "Could not find the LLVM interpreter \"$LLI\"."
  echo "Check your LLVM installation and/or modify the LLI variable in testall.sh"
  exit 1
}

which "$LLI" >> $globallog || LLIFail

if [ ! -f _build/builtins.o ]
then
    echo "Could not find builtins.o"
    echo "Have you run \"make\"?"
    exit 1
fi

if [ $# -ge 1 ]
then
    files=$@
else
    files="test/tests/test-*.bug test/tests/fail-*.bug"
    #files="test/tests/test-*.bug"
fi

for file in $files
do
    case $file in
	*test-*)
	    Check $file 2>> $globallog
	    ;;
	*fail-*)
	    CheckFail $file 2>> $globallog
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done

exit $globalerror
