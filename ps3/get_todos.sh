#!/bin/sh

echo "\n[   TODO   ]\n[==========]"

for file in `ls *.ml`; do
    echo "[----------] $file"
    fgrep -n TODO $file | awk '{print "[   >>>>   ]",$0}'
done

echo "[==========]"