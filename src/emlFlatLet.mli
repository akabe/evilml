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

type expr =
  {
    typ : EmlType.t;
    data : expr_desc;
  } [@@deriving show]
and expr_desc =
  | Error
  | Const of EmlSyntax.const
  | Var of string
  | If of expr * expr * expr
  | Op of expr EmlOp.t
  | Tuple of expr list
  | Constr of string * expr list
  | App of expr * expr list
  | Box of expr
  | Unbox of expr
  | Tag of expr (* Obtain the tag of a data constructor *)
  | Proj of expr * int * int (* Projection operator *)

and let_expr = let_expr_desc list * expr [@@deriving show]
and let_expr_desc =
  | Let_val of string * EmlType.scheme * expr
  | Let_fun of bool * string * EmlType.scheme * string option list * let_expr

type top =
  | Top_type of EmlType.decl
  | Top_let of let_expr_desc
  | Top_code of string [@@deriving show]

val convert : EmlBoxing.top list -> top list
