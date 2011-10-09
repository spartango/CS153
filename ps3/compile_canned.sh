#!/bin/sh

echo "[==========] Compiling Canned Cish Tests"

rm -rf compiled_tests

mkdir compiled_tests

if [[ -e ps2 ]]; then
	for filename in `ls test/*.cish`; do
		target=${filename%.cish}
		output_file="compiled_tests/${target:5}.asm"

		echo "\x1b\x5b1;36m[ COMPILE  ]\x1b\x5b0m ${filename:5}"
		# Tags on the debug print stuff at the header of the program so we can actually test

		# Compile
		./ps3 $filename > $output_file

		# Tag on little bits to print out the results of the program
		./inject.py $output_file > ${output_file%.asm}_test.asm

	done 
else 
	echo "\x1b\x5b1;31m[  ERROR   ]\x1b\x5b0m: Compiler  not found"
fi

echo "[==========] Complete"