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

type pattern = pattern_desc EmlTypedExpr.typed [@@deriving show]
and pattern_desc =
  | Pvar of string option
  | Pconst of EmlSyntax.const_pattern
  | Ptuple of pattern list
  | Pconstr of EmlType.constr_tag * string * pattern list

type expr = ext_expr EmlTypedExpr.base_expr
and ext_expr =
  | Match of expr * (pattern * expr) list
  | Constraint of expr * EmlType.t
                    [@@deriving show]

type top = ext_expr EmlTypedExpr.base_top [@@deriving show]

val typing : EmlContext.t -> EmlSyntax.top list -> top list
