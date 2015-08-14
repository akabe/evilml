(* Example: quick sort *)

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

let l1 = [5; 4; 8; 1; 6; 3; 7; 2]
let l2 = qsort l1
let x0 = nth 0 l2
let x1 = nth 1 l2
let x2 = nth 2 l2
let x3 = nth 3 l2
let x4 = nth 4 l2
let x5 = nth 5 l2
let x6 = nth 6 l2
let x7 = nth 7 l2

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
