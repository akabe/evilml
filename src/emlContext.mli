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

type t

val empty : t

val add_var : string -> EmlType.scheme -> t -> t

val add_args : string option list -> EmlType.t list -> t -> t

val add_type : EmlType.decl -> t -> t

val lookup_var : loc:EmlLocation.t -> string -> t -> EmlType.scheme

val lookup_constr :
  loc:EmlLocation.t -> string -> t ->
  EmlType.constr_tag * EmlType.t list * EmlType.t

val lookup_type : loc:EmlLocation.t -> string -> t -> EmlType.decl

val fv_in_context : t -> EmlType.VarSet.t

val generalize_type : t -> EmlType.t -> EmlType.scheme
