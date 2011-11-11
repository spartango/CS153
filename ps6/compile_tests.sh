#!/bin/sh
# Compiles a bunch of tests

# Clean
rm -rf test_output
mkdir -p test_output

if [[ -ne monadic ]]; then
	echo "Failed to find compiler"
else
	for test in `ls tests/*.ml`; do
		testname=${test%"tests/"}
		echo "[ COMPILE  ]: "$testname
		logfile=test_output/${testname%".ml"}.out
		./monadic $test > $logfile
		output=`tail -n 1 $logfile`
		echo "[  OUTPUT  ]: "$output
	done 
fi