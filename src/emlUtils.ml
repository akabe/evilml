(* Evil ML --- A compiler from ML to C++ template language

   Copyright (C) 2015 Akinori ABE <abe@sf.ecei.tohoku.ac.jp>

   Evil ML is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Evil ML is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

open Format

let identity x = x

let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)

module Option =
struct
  let map f = function
    | Some x -> Some (f x)
    | None -> None

  let default x = function
    | Some y -> y
    | None -> x
end

module String =
struct
  include String

  let has_prefix prefix s =
    let m = String.length prefix in
    let n = String.length s in
    if m <= n then String.sub s 0 m = prefix else false

  let to_list s =
    let rec aux i l = if i < 0 then l else aux (i-1) (s.[i] :: l) in
    aux (String.length s - 1) []
end

module List =
struct
  include List

  let is_empty xs = xs = []

  let init f n =
    let rec aux acc i = if i < 0 then acc else aux (f i :: acc) (i - 1) in
    aux [] (n - 1)

  let rec last = function
    | [] -> failwith "List.last"
    | [x] -> x
    | _ :: xs -> last xs

  (** [block n [x(1); x(2); ...; x(n); x(n+1); ...; x(m)]] returns
      [[x(1); x(2); ...; x(n)]] and [[x(n+1); ...; x(m)]]. *)
  let block n l =
    let rec aux acc n l =
      if n = 0 then (List.rev acc, l)
      else match l with
        | [] -> failwith "List.block"
        | hd :: tl -> aux (hd :: acc) (n-1) tl
    in
    aux [] n l

  let fold_righti f x init =
    fold_right (fun xi (i, acc) -> (i - 1, f i xi acc)) x (length x - 1, init)
    |> snd

  let findi f x =
    let rec aux i = function
      | [] -> None
      | hd :: tl -> if f hd then Some (i, hd) else aux (i+1) tl
    in
    aux 0 x

  let rev_fold_map f init x =
    fold_left
      (fun (acc, rev_x) xi ->
         let (acc', xi') = f acc xi in
         (acc', xi' :: rev_x))
      (init, []) x

  let fold_map f init x =
    let (acc, rev_x) = rev_fold_map f init x in
    (acc, rev rev_x)

  let filter_map f x =
    fold_left (fun acc xi ->
        match f xi with
        | Some yi -> yi :: acc
        | None -> acc)
      [] x
    |> rev

  let find_map f =
    let rec aux = function
      | [] -> None
      | xi :: x ->
        match f xi with
        | Some yi -> Some yi
        | None -> aux x
    in
    aux

  let duplicated l =
    let rec aux acc = function
      | [] -> acc
      | x :: l -> aux (if mem x l && not (mem x acc) then x :: acc else acc) l
    in
    rev (aux [] l)
end

module Format =
struct
  include Format

  let rec pp_list ~pp_delim pp ppf = function
    | [] -> ()
    | [x] -> pp ppf x
    | x :: l ->
      pp ppf x;
      pp_delim ppf;
      pp_list ~pp_delim pp ppf l

  let pp_list_comma pp =
    let pp_delim ppf = pp_print_char ppf ',' ; pp_print_space ppf () in
    pp_list ~pp_delim pp

  type buffer_formatter = { buffer : Buffer.t; ppf : formatter; }

  let create_buffer_formatter n =
    let buffer = Buffer.create n in
    let ppf = formatter_of_buffer buffer in
    { buffer; ppf; }

  let fetch_buffer_formatter bf =
    pp_print_flush bf.ppf ();
    Buffer.contents bf.buffer

  let skfprintf k fmt =
    let bf = create_buffer_formatter 16 in
    let aux ppf = k (fetch_buffer_formatter bf) in
    kfprintf aux bf.ppf fmt

  let sfprintf fmt = skfprintf (fun s () -> s) fmt
end

module StringSet = Set.Make(struct
    type t = string
    let compare = Pervasives.compare
  end)

let gen_fresh_name prefix =
  let c = ref 0 in
  fun () -> incr c ; prefix ^ string_of_int !c

let read_file fname =
  let open Buffer in
  let ic = open_in fname in
  let b = create 256 in
  try
    while true do
      add_string b (input_line ic);
      add_char b '\n'
    done;
    assert false
  with End_of_file ->
    close_in ic;
    contents b

exception Compile_error of string EmlLocation.loc

let errorf ?(loc = EmlLocation.dummy) fmt =
  Format.skfprintf
    (fun s -> raise (Compile_error EmlLocation.({ loc; data = s; })))
    fmt
