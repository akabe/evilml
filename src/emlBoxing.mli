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

type expr = ext_expr EmlTypedExpr.base_expr [@@deriving show]
and ext_expr =
  | Box of expr
  | Unbox of expr
  | Tag of expr (* Obtain the tag of a data constructor *)
  | Proj of expr * int (* Projection operator *)

type top = ext_expr EmlTypedExpr.base_top [@@deriving show]

val convert :
  EmlType.context ->
  EmlRemoveMatch.top list ->
  top list
