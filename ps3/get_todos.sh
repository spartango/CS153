#!/bin/sh

echo "[   TODO   ]\n[==========]"

for file in `ls *.ml`; do
    echo "[----------] $file"
    fgrep TODO $file 
done

echo "[==========]"