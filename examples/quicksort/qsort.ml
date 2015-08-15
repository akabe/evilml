(* Example: quick sort *)

#use "list.ml"

let rec qsort xs = match xs with
  | [] -> []
  | [x] -> [x]
  | pivot :: rest ->
    match list_partition (fun x -> x < pivot) rest with
    | (ys, zs) -> list_append (qsort ys) (pivot :: qsort zs)

let l1 = [5; 4; 8; 1; 6; 3; 7; 2]
let l2 = qsort l1
let x0 = list_nth l2 0
let x1 = list_nth l2 1
let x2 = list_nth l2 2
let x3 = list_nth l2 3
let x4 = list_nth l2 4
let x5 = list_nth l2 5
let x6 = list_nth l2 6
let x7 = list_nth l2 7

(*!
// This is C++ code.

#include <cstdio>

int main () { // We use printf in order to output readable assembly code.
  std::printf("%d  ", x0::val);
  std::printf("%d  ", x1::val);
  std::printf("%d  ", x2::val);
  std::printf("%d  ", x3::val);
  std::printf("%d  ", x4::val);
  std::printf("%d  ", x5::val);
  std::printf("%d  ", x6::val);
  std::printf("%d\n", x7::val);
  return 0;
}
*)
