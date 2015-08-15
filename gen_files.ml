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

let make_ml in_file out_file =
  let s = read_file in_file in
  let oc = open_out out_file in
  let ppf = formatter_of_out_channel oc in
  fprintf ppf "let contents = %S@." s;
  pp_print_flush ppf ();
  close_out oc

let () =
  make_ml "include/evilml.hpp" "src/evilml_hpp.ml";
  make_ml "examples/fib/fib.ml" "src/example_fib.ml";
  make_ml "examples/quicksort/qsort.ml" "src/example_qsort.ml"
