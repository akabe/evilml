let rec fib n = match n with
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n-1) + fib (n-2)

let x1 = fib 1
let x2 = fib 2
let x3 = fib 3
let x4 = fib 4
let x5 = fib 5
let x6 = fib 6
let x7 = fib 7
let x8 = fib 8
let x9 = fib 9

(*!
// This is C++ code.

#include <cassert>

int main () { // We use printf in order to output readable assembly code.
  // check the unsorted list
  assert(x1::val == 1);
  assert(x2::val == 1);
  assert(x3::val == 2);
  assert(x4::val == 3);
  assert(x5::val == 5);
  assert(x6::val == 8);
  assert(x7::val == 13);
  assert(x8::val == 21);
  assert(x9::val == 34);
  return 0;
}
*)
