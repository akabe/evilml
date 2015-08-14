(* Example: Fibonacci numbers *)

let rec fib n = match n with
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n-1) + fib (n-2)

let x = fib 10

(*!
// This is C++ code.

#include <cstdio>

int main () { // We use printf in order to output readable assembly code.
  std::printf("fib 10 = %d\n", x::val); // fib 10 = 55
  return 0;
}
*)
