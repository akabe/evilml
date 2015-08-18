#!/bin/sh

set -eu
#EVILML="../evilml.native -I ../include --header evilml.hpp --embed --verbose"
EVILML="../evilml.native -I ../include --header ../include/evilml.hpp --embed --verbose"

export OCAMLRUNPARAM=b

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

echo "Checking tsort.ml ..."
$EVILML tsort.ml
g++ tsort.cpp -o tsort.out
./tsort.out

echo "Checking base64.ml ..."
$EVILML base64.ml
g++ base64.cpp -o base64.out
./base64.out
