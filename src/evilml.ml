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

(** Build-in functions and their types *)
let builtin_ctx =
  [
    "succ", Type.Arrow ([Type.Int], Type.Int);
    "pred", Type.Arrow ([Type.Int], Type.Int);
    "min", Type.Arrow ([Type.Int; Type.Int], Type.Int);
    "max", Type.Arrow ([Type.Int; Type.Int], Type.Int);
    "int_of_char", Type.Arrow ([Type.Int], Type.Char);
    "char_of_int", Type.Arrow ([Type.Char], Type.Int);
  ]
  |> List.map (fun (id, t) -> (id, Type.scheme t))

(** Build-in functions and their real names *)
let builtin_tbl =
  [
    "succ", "__ml_succ";
    "pred", "__ml_pred";
    "min", "__ml_min";
    "max", "__ml_max";
    "int_of_char", "__ml_int_of_char";
    "char_of_int", "__ml_char_of_int";
  ]

(** Set an input filename to `lexbuf'. *)
let set_lexbuf lexbuf fname =
  let open Lexing in
  lexbuf.lex_curr_p <- { pos_fname = fname; pos_lnum = 1;
                         pos_bol = 0; pos_cnum = 0; }

let make_header in_fname =
  let hpp_fname = "evilml.hpp" in
  function
  | false -> sprintf "#include %S" hpp_fname
  | true -> sprintf "#line 1 %S\n%s\n#line 1 %S"
              hpp_fname Evilml_hpp.contents in_fname

let compile ~embed in_fname in_code =
  let bf_tys = create_buffer_formatter 1024 in
  let bf_out = create_buffer_formatter 1024 in
  let header = make_header in_fname embed in
  Lexing.from_string in_code (* Create `lexbuf' for lexing *)
  |> (fun lexbuf -> set_lexbuf lexbuf in_fname ; lexbuf)
  |> Parser.main Lexer.main (* parsing *)
  |> Typing.typing builtin_ctx (* type inference *)
  |> (fun tops -> (* Hook: obtain the result of type inference *)
      List.iter (fun top -> match top.Location.data with
          | TypedExpr.Top_let (_, id, ts, _) ->
            fprintf bf_tys.ppf "val %s : %a@." id Type.pp_scheme ts
          | _ -> ()) tops;
      tops)
  |> RemoveMatch.convert (* Convert match-expressions into if-expressions *)
  |> UnCurrying.convert (* UnCurrying functions *)
  |> Assoc.convert (* Transformation for C++ *)
  |> Boxing.convert builtin_ctx (* Insert boxing/unboxing *)
  |> (fun tops -> Alpha.convert (* Alpha conversion (renaming identifiers) *)
         (Alpha.make_renamer builtin_tbl tops) tops)
  |> FlatLet.convert (* Flatten let-expressions *)
  |> Cpp.convert ~header (* Convert ML code into C++ template code *)
  |> List.iter (fprintf bf_out.ppf "%a@\n@\n" Cpp.pp_decl);
  (fetch_buffer_formatter bf_tys |> String.trim,
   fetch_buffer_formatter bf_out |> String.trim)

open Js
open Dom_html

let editor_get id = to_string (Unsafe.variable id)##getDoc()##getValue()
let editor_set id s = (Unsafe.variable id)##getDoc()##setValue(string s)

let input id =
  match tagged (getElementById id) with
  | Input x -> x
  | _ -> failwith "Not <input> element"

let report_error loc msg =
  editor_set "cppEditor" (sfprintf "%a@\nError: %s" Location.pp loc msg ());
  match loc with
  | None -> ()
  | Some loc ->
    Unsafe.fun_call (Unsafe.js_expr "reportError")
      [| Unsafe.inject (loc.Location.lnum_start);
         Unsafe.inject (loc.Location.cnum_start);
         Unsafe.inject (loc.Location.lnum_end);
         Unsafe.inject (loc.Location.cnum_end);
         Unsafe.inject (string msg); |]

let onclick _ =
  let embed = to_bool (input "chk_embed")##checked in
  let in_code = editor_get "mlEditor" in
  begin
    try
      let (tyinf, out_code) = compile ~embed "(none)" in_code in
      Unsafe.fun_call (Unsafe.js_expr "showResult")
        [| Unsafe.inject (string tyinf);
           Unsafe.inject (string out_code); |]
    with
    | Compile_error ({ Location.loc; Location.data; }) -> report_error loc data
  end;
  bool true

let _ =
  let btn = getElementById "btn_compile" in
  addEventListener btn Event.click (Dom.handler onclick) (bool false)
