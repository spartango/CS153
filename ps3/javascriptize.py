#!/usr/bin/python

import sys
import os 

# Reads file, outputting to stdout, and invoking injection on pattern match
def search_file(filename):
    # Scope tracking
    depth  = 0
    placed = False

    # Pull open file
    t_file = open(filename, 'r')

    # Line by line read
    for line in t_file:
        # Conditional inject
        if depth == 0 and not placed and not line.strip(" \n\t") == "":
            # Line check for return
            print "function ",
            placed = True
        
        if line.find("{") != -1:
            depth = depth +1 
            placed = False
        
        if line.find("}") != -1:
            depth = depth -1
            placed = False;
                    
        print line.replace("let ", "var "),


if(len(sys.argv) > 1):
    filename = sys.argv[1]
    search_file(filename)
else:
    print "File not found"
    exit(1)