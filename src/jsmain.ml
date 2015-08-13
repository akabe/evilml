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

open Utils
open Format
open Js
open Dom_html

let compile in_code =
  let b = Buffer.create 256 in
  let ppf = formatter_of_buffer b in
  try
    Lexing.from_string in_code
    |> Compile.run ~header:"#include \"evilml.hpp\"" "(none)"
    |> List.iter (fprintf ppf "%a@\n@." Cpp.pp_decl);
    pp_print_flush ppf ();
    Buffer.contents b
  with
  | Compile_error ({ Location.loc; Location.data; }) ->
    sfprintf "%a@\nError: %s@." Location.pp loc data ()

let onclick _ =
  let code = (Unsafe.variable "ocamlEditor")##getDoc()##getValue()
             |> to_string
             |> compile in
  (Unsafe.variable "cppEditor")##getDoc()##setValue(string code);
  bool true

let _ =
  let btn = getElementById "btn_compile" in
  addEventListener btn Event.click (Dom.handler onclick) (bool false)
