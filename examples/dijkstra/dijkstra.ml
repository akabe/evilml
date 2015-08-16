(** Example: Dijkstra's algorithm *)

#use "list.ml"

let remove_worse_paths ps =
  let eq x y = match (x, y) with ((vx, _, _), (vy, _, _)) -> vx = vy in
  let rec aux ps = match ps with
    | [] -> []
    | x :: ps -> match list_partition (eq x) ps with (_, ps) -> x :: aux ps
  in
  aux ps

let insert xs x =
  let cost p = match p with (v, path, c) -> c in
  let c = cost x in
  let rec aux xs = match xs with
    | [] -> [x]
    | hd :: tl -> if cost hd < c then hd :: (aux tl) else x :: xs
  in
  aux xs

let walk graph ps =
  let mk_path p x = match (p, x) with
    | ((v, path, cp), (v1, v2, ce)) ->
      if v = v1 then Some (v2, v1 :: path, cp + ce) else None
  in
  match ps with
  | [] -> error
  | p :: ps1 ->
    let ps2 = list_filter_map (mk_path p) graph in
    let ps = list_foldl insert ps1 ps2 in
    remove_worse_paths ps

let dijkstra graph goal start =
  let is_goal x = match x with (v, _, _) -> v = goal in
  let rec aux ps =
    match list_find is_goal ps with
    | Some p -> p
    | None -> aux (walk graph ps)
  in
  match aux [(start, [], 0)] with (v, p, c) -> (list_rev (v :: p), c)

(*  +----------> [6] <-----------+
  9 |                            | 6
   [5] <-------- [3] ---------> [4]
    ^    2      ^  ^       11    ^
    |         9 |  |  10         |
 14 |       +---+  +-----+       | 15
    |       |    7       |       |
    +----- [1] -------> [2] -----+ *)
let graph = [ (1, 2, 7); (* (vertex_begin, vertex_end, cost) *)
              (1, 3, 9);
              (1, 5, 14);
              (2, 3, 10);
              (2, 4, 15);
              (3, 4, 11);
              (3, 5, 2);
              (4, 6, 6);
              (5, 6, 9) ]

let fst x = match x with (y, _) -> y
let snd x = match x with (_, y) -> y

(* the shortest path = 1 -> 3 -> 5 -> 6 (its cost = 20) *)
let res = dijkstra graph 6 1
let path = fst res
let cost = snd res
let v0 = list_nth path 0
let v1 = list_nth path 1
let v2 = list_nth path 2
let v3 = list_nth path 3

(*!
// This is C++ code.

#include <cstdio>

int main () { // We use printf in order to output readable assembly code.
  std::printf("cost = %d\n", cost::val);
  std::printf("%d -> ", v0::val);
  std::printf("%d -> ", v1::val);
  std::printf("%d -> ", v2::val);
  std::printf("%d\n", v3::val);
  return 0;
}
*)
