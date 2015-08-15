#use "option.ml"

type 'a list = [] | :: of 'a * 'a list

let rec list_map f xs = match xs with
  | [] -> []
  | x :: xs -> f x :: list_map f xs

let rec list_foldl f acc xs = match xs with
  | [] -> acc
  | x :: xs -> list_foldl f (f acc x) xs

let rec list_foldr f xs acc = match xs with
  | [] -> acc
  | x :: xs -> f x (list_foldr f xs acc)

let rec list_nth xs i = match xs with
  | [] -> error
  | x :: xs -> if i = 0 then x else list_nth xs (i-1)

let list_length = list_foldl (fun n _ -> n + 1) 0
let list_rev = list_foldl (fun acc x -> x :: acc) []
let list_append = list_foldr (fun x acc -> x :: acc)

let list_filter f xs =
  list_foldr (fun x acc -> if f x then x :: acc else acc) xs []

let list_partition f xs =
  list_foldr (fun x acc -> match acc with
      | (ys, zs) -> if f x then (x :: ys, zs) else (ys, x :: zs))
    xs ([], [])

let rec list_find f xs = match xs with
  | [] -> None
  | x :: xs -> if f x then Some x else list_find f xs

let list_mem x xs = list_find (fun y -> x = y) xs <> None

let rec list_assoc x xs = match xs with
  | [] -> None
  | (y, z) :: xs -> if x = y then Some z else list_assoc x xs
