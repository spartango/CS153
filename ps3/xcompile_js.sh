#!/bin/sh

echo "[==========] Cross-Compiling Canned Cish Tests To Javascript"

rm -rf compiled_js

mkdir compiled_js

if [[ -e javascriptize.py ]]; then
    for filename in `ls test/*.cish`; do
        target=${filename%.cish}
        output_file="compiled_js/${target:5}.js"

        echo "\x1b\x5b1;36m[ XCOMPILE ]\x1b\x5b0m ${filename:5}"
        # Tags on the debug print stuff at the header of the program so we can actually test

        # Compile
        ./javascriptize.py $filename > $output_file

        # Tag on little bits to print out the results of the program
        ./injectjsconsole.py $output_file > ${output_file%.js}_test.js

    done 
else 
    echo "\x1b\x5b1;31m[  ERROR   ]\x1b\x5b0m: Compiler  not found"
fi

echo "[==========] Complete"