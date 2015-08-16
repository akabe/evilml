(** Example: topological sort *)

#use "list.ml"

let tsort vs es =
  let is_leaf es v =
    list_for_all (fun e -> match e with (_, v2) -> v <> v2) es
  in
  let partition_leaves vs es =
    list_partition (fun e -> match e with (v, _) -> list_mem v vs) es
  in
  let rec aux acc vs es =
    match list_partition (is_leaf es) vs with
    | (vs1, []) -> list_flatten (list_rev (vs1 :: acc))
    | (vs1, vs2) ->
      match partition_leaves vs1 es with (_, es2) -> aux (vs1 :: acc) vs2 es2
  in
  aux [] vs es

(*  +----> [2] --> [5] <-- [7]
    |               ^       ^
    |               |       |
   [1] <-- [3] -----+       |
    ^       ^               |
    |       |               |
    +----- [4] <---------- [6] *)
let vertices = [1; 2; 3; 4; 5; 6; 7]
let edges = [ (1, 2); (* (vertex_begin, vertex_end) *)
              (2, 5);
              (3, 1);
              (3, 5);
              (4, 1);
              (4, 3);
              (6, 4);
              (6, 7);
              (7, 5) ]

(* Result: 6, 4, 7, 3, 1, 2, 5 *)
let xs = tsort vertices edges
let x0 = list_nth xs 0
let x1 = list_nth xs 1
let x2 = list_nth xs 2
let x3 = list_nth xs 3
let x4 = list_nth xs 4
let x5 = list_nth xs 5
let x6 = list_nth xs 6

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
  std::printf("%d\n", x6::val);
  return 0;
}
*)
