#!/bin/sh
# Compiles a bunch of tests

# Clean
rm -rf output_tests
mkdir -p output_tests
echo "[==========]"
for test in `ls tests/*.ml`; do
	testname=${test}
	echo "[ COMPILE  ]: "$testname
	logfile=output_${testname%".ml"}.out
	./monadic $test $@ > $logfile
	output=`tail -n 2 $logfile`
	echo "[  OUTPUT  ]: "$output
done 
echo "[==========]"