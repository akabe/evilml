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

let builtin_tbl =
  [
    "succ", "__ml_succ";
    "pred", "__ml_pred";
    "min", "__ml_min";
    "max", "__ml_max";
    "int_of_char", "__ml_int_of_char";
    "char_of_int", "__ml_char_of_int";
  ]

let print_type tops =
  printf "[Typing]@.";
  List.iter (fun top -> match top.Location.data with
      | TypedExpr.Top_let (_, id, ts, _) ->
        printf "val %s :: %a@." id Type.pp_scheme ts
      | _ -> ()) tops

let set_lexbuf lexbuf fname =
  let open Lexing in
  lexbuf.lex_curr_p <- { pos_fname = fname; pos_lnum = 1;
                         pos_bol = 0; pos_cnum = 0; }

let run ?(verbose = false) ~header fname lexbuf =
  set_lexbuf lexbuf fname;
  lexbuf
  |> Parser.main Lexer.main
  |> Typing.typing builtin_ctx
  |> (fun tops -> if verbose then print_type tops ; tops)
  |> RemoveMatch.convert
  |> UnCurrying.convert
  |> Assoc.convert
  |> Boxing.convert builtin_ctx
  |> (fun tops -> Alpha.convert (Alpha.make_renamer builtin_tbl tops) tops)
  |> FlatLet.convert
  |> Cpp.convert ~header
