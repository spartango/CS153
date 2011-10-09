#!/bin/sh

echo "\n[==========]\n[   TODO   ]"

for file in `ls *.ml`; do
    echo "[----------] $file"
    fgrep -n TODO $file | awk '{print "[   >>>>   ]",$0}'
done

echo "[==========]\n"