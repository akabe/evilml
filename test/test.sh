#!/bin/sh

set -eu

EVILML="../evilml.native --embed --verbose"

echo "Checking fib.ml ..."
$EVILML fib.ml
g++ fib.cpp -o fib.out
./fib.out

echo "Checking qsort.ml ..."
$EVILML qsort.ml
g++ qsort.cpp -o qsort.out
./qsort.out
