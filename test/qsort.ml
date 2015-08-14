type 'a list = [] | :: of 'a * 'a list

let rec foldr f xs acc = match xs with
  | [] -> acc
  | x :: xs -> f x (foldr f xs acc)

let length xs = foldr (fun _ acc -> acc + 1) xs 0
let append xs ys = foldr (fun y acc -> y :: acc) xs ys
let filter f xs = foldr (fun x acc -> if f x then x :: acc else acc) xs []

let rec qsort xs = match xs with
  | [] -> []
  | [x] -> [x]
  | pivot :: rest ->
    let ys = qsort (filter (fun x -> x < pivot) rest) in
    let zs = qsort (filter (fun x -> x >= pivot) rest) in
    append ys (pivot :: zs)

let rec nth i xs = match xs with
  | [] -> error
  | x :: xs -> if i = 0 then x else nth (i-1) xs

let xs = [5; 4; 8; 1; 6; 3; 7; 2]
let x0 = nth 0 xs
let x1 = nth 1 xs
let x2 = nth 2 xs
let x3 = nth 3 xs
let x4 = nth 4 xs
let x5 = nth 5 xs
let x6 = nth 6 xs
let x7 = nth 7 xs
let ys = qsort xs
let y0 = nth 0 ys
let y1 = nth 1 ys
let y2 = nth 2 ys
let y3 = nth 3 ys
let y4 = nth 4 ys
let y5 = nth 5 ys
let y6 = nth 6 ys
let y7 = nth 7 ys

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
