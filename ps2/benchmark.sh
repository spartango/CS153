#!/bin/sh

echo "[==========] Benchmarking Canned Tests"

if [[ -e compiled_tests ]]; then
	for test_file in `ls compiled_tests/*_test.asm`; do
		echo "\x1b\x5b1;36m[ RUNNING  ]\x1b\x5b0m ${test_file:15}"
		log_file=${test_file%.asm}.log
		(time ./spim_run.sh $test_file > $log_file) 2>> $log_file 
		count=`wc -l $log_file | awk '{print $1}'`
		if (($count > 6)); then
			# Something has failed, we'll log out and return a nice fail message
			echo "\x1b\x5b1;31m[  FAILED  ]\x1b\x5b0m See $log_file for error message"
		else
			echo "\x1b\x5b1;32m[ COMPLETE ]\x1b\x5b0m Time:" `tail -n 2 $log_file | head -n 1`
		fi
	done
else
	echo "\x1b\x5b1;31m[  ERROR   ]\x1b\x5b0m: Compiled Tests not found"
fi

echo "[==========] Completed"