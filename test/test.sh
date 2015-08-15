#!/bin/sh

set -eu

EVILML="../evilml.native --embed --verbose"

echo "Checking fib.ml ..."
$EVILML fib.ml
cat fib.cpp
g++ fib.cpp -o fib.out
./fib.out

echo "Checking qsort.ml ..."
$EVILML qsort.ml
cat qsort.cpp
g++ qsort.cpp -o qsort.out
./qsort.out
