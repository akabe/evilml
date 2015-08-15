#use "list.ml"

let rec qsort xs = match xs with
  | [] -> []
  | [x] -> [x]
  | pivot :: rest ->
    match list_partition (fun x -> x < pivot) rest with
    | (ys, zs) -> list_append (qsort ys) (pivot :: qsort zs)

let xs = [5; 4; 8; 1; 6; 3; 7; 2]
let x0 = list_nth xs 0
let x1 = list_nth xs 1
let x2 = list_nth xs 2
let x3 = list_nth xs 3
let x4 = list_nth xs 4
let x5 = list_nth xs 5
let x6 = list_nth xs 6
let x7 = list_nth xs 7
let ys = qsort xs
let y0 = list_nth ys 0
let y1 = list_nth ys 1
let y2 = list_nth ys 2
let y3 = list_nth ys 3
let y4 = list_nth ys 4
let y5 = list_nth ys 5
let y6 = list_nth ys 6
let y7 = list_nth ys 7

(*!
// This is C++ code.

#include <cassert>

int main () { // We use printf in order to output readable assembly code.
  // check the unsorted list
  assert(x0::val == 5);
  assert(x1::val == 4);
  assert(x2::val == 8);
  assert(x3::val == 1);
  assert(x4::val == 6);
  assert(x5::val == 3);
  assert(x6::val == 7);
  assert(x7::val == 2);
  // check the sorted list
  assert(y0::val == 1);
  assert(y1::val == 2);
  assert(y2::val == 3);
  assert(y3::val == 4);
  assert(y4::val == 5);
  assert(y5::val == 6);
  assert(y6::val == 7);
  assert(y7::val == 8);
  return 0;
}
*)
