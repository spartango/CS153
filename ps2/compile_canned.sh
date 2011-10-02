#!/bin/sh

echo "[==========] Compiling Canned Fish Tests"

rm -rf compiled_tests

mkdir compiled_tests

if [[ -e ps2 ]]; then
	for filename in `ls test/*.fish`; do
		target=${filename%.fish}
		output_file="compiled_tests/${target:5}.asm"

		echo "\x1b\x5b1;36m[COMPILING ]\x1b\x5b0m ${filename:5}"
		# Tags on the debug print stuff at the header of the program so we can actually test
		cat print.asm > $output_file
		echo "\n" >> $output_file
		# Compile
		./ps2 $filename >> $output_file

		# Tag on little bits to print out the results of the program
		awk -v modified="${output_file%.asm}_test.asm" '{sub(/jr\t\$31/,"move $a0, $2\n\tj printInt");print > modified}' $output_file

		# We don't need to keep the leftovers
		rm $output_file
	done 
else 
	echo "\x1b\x5b1;31m[  ERROR   ]\x1b\x5b0m: Compiler  not found"
fi

echo "[==========] Complete"