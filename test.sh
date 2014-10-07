#!/bin/bash
set -e
ghc --make stupid
./stupid data.txt > myout
diff myout results.txt
