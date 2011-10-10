#!/bin/sh

echo "[==========] Comparting Canned Test Results"

if [[ -e compiled_tests && -e compiled_js ]]; then
    for test_file in `ls compiled_tests/*_test.asm`; do
        echo "\x1b\x5b1;36m[ RUNNING  ]\x1b\x5b0m ${test_file:15}"
        # Run the assembly version
        log_file=${test_file%.asm}.log
        ./spim_run.sh $test_file > $log_file 2>&1

        #run the javascript version
        js_file="compiled_js/${test_file:15}"
        jslog_file=${js_file%.asm}.log
        node ${js_file%.asm}.js > $jslog_file 2>&1

        count=`wc -l $log_file | awk '{print $1}'`
        if (($count > 2)); then
            # Something has failed, we'll log out and return a nice fail message
            echo "\x1b\x5b1;31m[  FAILED  ]\x1b\x5b0m See $log_file for error message"
        else
            echo "\x1b\x5b1;32m[ COMPLETE ]\x1b\x5b0m Returned:" "\x1b\x5b1;36m" `tail -n 1 $jslog_file` "\x1b\x5b0m" " vs "  `tail -n 1 $log_file`
        fi
    done
else
    echo "\x1b\x5b1;31m[  ERROR   ]\x1b\x5b0m: Compiled Tests not found"
fi

echo "[==========] Completed"