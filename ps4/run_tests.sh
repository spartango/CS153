#!/bin/bash

echo "[  TESTS   ]"
echo "[==========]"
for file in `ls test/*.scish`; do
  echo -n "[   RUN    ] "
  ./ps4_scish $file | ./ps4_cish_stdin | grep answer
done
echo "[==========]"
echo "[ COMPLETE ]"