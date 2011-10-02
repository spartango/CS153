#!/bin/sh

echo "Running Canned Tests\n---"

if [[ -e compiled_tests ]]; then
	for test_file in `ls compiled_tests/*_test.asm`; do
		echo "[ RUNNING  ] $test_file"
		result=`./spim_run.sh $test_file`
		echo "[ COMPLETE ] Returned: $result"
	done
else
	echo "Error: Compiled Tests not found"
fi

echo "---\nCompleted"