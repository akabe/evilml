#!/bin/sh

set -eu
#EVILML="../evilml.native -I ../include --header evilml.hpp --embed --verbose"
EVILML="../evilml.native -I ../include --header ../include/evilml.hpp --embed --verbose"

echo "Checking fib.ml ..."
$EVILML fib.ml
g++ fib.cpp -o fib.out
./fib.out

echo "Checking qsort.ml ..."
$EVILML qsort.ml
g++ qsort.cpp -o qsort.out
./qsort.out

echo "Checking dijkstra.ml ..."
$EVILML dijkstra.ml
g++ dijkstra.cpp -o dijkstra.out
./dijkstra.out
