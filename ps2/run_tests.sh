#!/bin/sh

echo "Running Canned Tests\n---"

if [[ -e compiled_tests ]]; then
	for test_file in `ls compiled_tests/*_test.asm`; do
		echo "[ RUNNING  ] ${test_file:15}"
		log_file=${test_file%.asm}.log
		./spim_run.sh $test_file  > $log_file 2>&1
		count=`wc -l $log_file | awk '{print $1}'`
		if (($count > 2)); then
			# Something has failed, we'll log out and return a nice fail message
			echo "[  FAILED  ] See $log_file for error message"
		else
			echo "[ COMPLETE ] Returned:" `tail -n 1 $log_file`
		fi
	done
else
	echo "Error: Compiled Tests not found"
fi

echo "---\nCompleted"