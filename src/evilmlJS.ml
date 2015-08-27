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

open EmlUtils
open Format
open Js
open Dom_html

let input_fname = "(none)"

let editor_get id = to_string (Unsafe.variable id)##getDoc##getValue
let editor_set id s = (Unsafe.variable id)##getDoc##setValue (string s)

let input id =
  match tagged (getElementById id) with
  | Input x -> x
  | _ -> failwith "Not <input> element"

let report_error loc msg =
  editor_set "cppEditor" (sfprintf "%a@\nError: %s" EmlLocation.pp loc msg ());
  match loc with
  | Some loc when loc.EmlLocation.fname = input_fname ->
    Unsafe.fun_call (Unsafe.js_expr "reportError")
      [| Unsafe.inject (loc.EmlLocation.lnum_start);
         Unsafe.inject (loc.EmlLocation.cnum_start);
         Unsafe.inject (loc.EmlLocation.lnum_end);
         Unsafe.inject (loc.EmlLocation.cnum_end);
         Unsafe.inject (string msg); |]
  | _ -> ()

let make_header embed =
  let hpp_fname = "evilml.hpp" in
  if embed
  then sprintf "#line 1 %S\n%s\n#line 1 \"output.cpp\""
      hpp_fname Evilml_hpp.contents
  else sprintf "#include %S" hpp_fname

let loader loc fname =
  match fname with
  | "option.ml" -> Lexing.from_string Option_ml.contents
  | "list.ml" -> Lexing.from_string List_ml.contents
  | _ -> errorf ~loc "File %S is not found" fname ()

let compile () =
  let embed = to_bool (input "chk_embed")##.checked in
  let in_code = editor_get "mlEditor" in
  let bf_tys = create_buffer_formatter 1024 in
  let bf_out = create_buffer_formatter 1024 in
  let hook_typing tops =
    List.iter (fun top -> match top.EmlLocation.data with
        | EmlTypedExpr.Top_let (_, id, ts, _) ->
          fprintf bf_tys.ppf "val %s : %a@." id EmlType.pp_scheme ts
        | _ -> ()) tops
  in
  begin
    try
      let lexbuf = Lexing.from_string in_code in
      EmlCompile.run
        ~loader ~hook_typing ~header:(make_header embed) input_fname lexbuf
      |> List.iter (fprintf bf_out.ppf "%a@\n@\n" EmlCpp.pp_decl);
      let tyinf = fetch_buffer_formatter bf_tys |> String.trim in
      let out_code = fetch_buffer_formatter bf_out |> String.trim in
      Unsafe.fun_call (Unsafe.js_expr "showResult")
        [| Unsafe.inject (string tyinf);
           Unsafe.inject (string out_code); |]
    with
    | Compile_error ({ EmlLocation.loc; EmlLocation.data; }) ->
      report_error loc data
  end

let switch_example code () =
  editor_set "mlEditor" code

let () =
  let set_onclick id f =
    let handler _ = f () ; bool true in
    let btn = getElementById id in
    ignore (addEventListener btn Event.click (Dom.handler handler) (bool false))
  in
  set_onclick "btn_compile" compile;
  set_onclick "btn_ex_fib" (switch_example Example_fib.contents);
  set_onclick "btn_ex_qsort" (switch_example Example_qsort.contents);
  set_onclick "btn_ex_tsort" (switch_example Example_tsort.contents);
  set_onclick "btn_ex_dijkstra" (switch_example Example_dijkstra.contents);
  set_onclick "btn_ex_base64" (switch_example Example_base64.contents)
