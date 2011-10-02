#!/bin/sh

echo "Compiling Canned Fish Tests\n---"

rm -rf compiled_tests

mkdir compiled_tests

if [[ -e ps2 ]]; then
	for filename in `ls test/*.fish`; do
		output=${filename%.fish}
		echo "Compiling: ${filename:4}"
		./ps2 $filename > compiled_tests/${output:4}.asm
	done 
fi

echo "---\nCompleted"