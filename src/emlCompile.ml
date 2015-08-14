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

(** Build-in functions and their types *)
let builtin_ctx =
  [
    "succ", EmlType.Arrow ([EmlType.Int], EmlType.Int);
    "pred", EmlType.Arrow ([EmlType.Int], EmlType.Int);
    "min", EmlType.Arrow ([EmlType.Int; EmlType.Int], EmlType.Int);
    "max", EmlType.Arrow ([EmlType.Int; EmlType.Int], EmlType.Int);
    "int_of_char", EmlType.Arrow ([EmlType.Int], EmlType.Char);
    "char_of_int", EmlType.Arrow ([EmlType.Char], EmlType.Int);
  ]
  |> List.map (fun (id, t) -> (id, EmlType.scheme t))

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

let run ?(hook_typing = ignore) ~embed in_fname lexbuf =
  let header = make_header in_fname embed in
  set_lexbuf lexbuf in_fname;
  lexbuf
  |> EmlParser.main EmlLexer.main (* parsing *)
  |> EmlTyping.typing builtin_ctx (* type inference *)
  |> (fun tops -> hook_typing tops ; tops) (* Hook typing results *)
  |> EmlRemoveMatch.convert (* Convert match-expressions into if-expressions *)
  |> EmlUnCurrying.convert (* EmlUnCurrying functions *)
  |> EmlAssoc.convert (* Transformation for C++ *)
  |> EmlBoxing.convert builtin_ctx (* Insert boxing/unboxing *)
  |> (fun tops -> (* EmlAlpha conversion (renaming identifiers) *)
      EmlAlpha.convert (EmlAlpha.make_renamer builtin_tbl tops) tops)
  |> EmlFlatLet.convert (* Flatten let-expressions *)
  |> EmlCpp.convert ~header (* Convert ML code into C++ template code *)
