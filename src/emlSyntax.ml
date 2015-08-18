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
open EmlUtils
open EmlLocation

type const_pattern =
  | Punit
  | Pbool of bool
  | Pchar of char
  | Pint of int
      [@@deriving show]

type pattern = pattern_desc EmlLocation.loc
and pattern_desc =
  | Pvar of string option
  | Pconst of const_pattern
  | Ptuple of pattern list
  | Pconstr of string * pattern list
      [@@deriving show]

type const =
  | Unit
  | Bool of bool
  | Char of char
  | Int of int
  | Float of float
      [@@deriving show]

type expr = expr_desc EmlLocation.loc
and expr_desc =
  | Const of const
  | Var of string
  | Constr of string * expr list
  | Tuple of expr list
  | If of expr * expr * expr
  | EmlOp of expr EmlOp.t
  | App of expr * expr list
  | Abs of string option list * expr
  | Let of bool * string * expr * expr
  | Match of expr * (pattern * expr) list
  | Constraint of expr * EmlType.t
  | Error
      [@@deriving show]

type top = top_desc EmlLocation.loc
and top_desc =
  | Top_variant_type of string
                        * string option list
                        * (string * EmlType.t list) list
  | Top_let of bool * string * expr
  | Top_code of string
  | Top_use of string
      [@@deriving show]
