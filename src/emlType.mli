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

type type_var [@@deriving show]

type t =
  | Unit
  | Bool
  | Char
  | Int
  | Float
  | Arrow of t list * t
  | Tuple of t list
  | Tconstr of string * t list
  | Var of string option * type_var
  | Ref of t ref (* for destructive unification *)

(** {2 Sets of type variables} *)

module VarSet : Set.S with type elt = type_var

(** {2 Types} *)

val fresh_type_var : unit -> type_var
val fresh_var : ?name:string -> unit -> t

val observe : t -> t
val is_basetype : t -> bool
val unarrow : t -> (t list * t) option
val box_type : t -> bool * t
val unbox_type : t -> bool * t

(** Returns a set of free type variables in a given type. *)
val fv_in_type : t -> VarSet.t

val unify : loc:EmlLocation.t -> t -> t -> unit

val pp : Format.formatter -> t -> unit

(** {2 Type schemes} *)

type scheme

(** [scheme t] converts type [t] into a type scheme with no generalization,
    i.e., no type variables are for-all bound. *)
val scheme : t -> scheme

(** [generalize set t] generalizes type [t] into a type scheme by substituting
    type variables in [set] for fresh variables. *)
val generalize : VarSet.t -> t -> scheme

(** [instantiate ts] instantiates for-all bound type variables in type scheme
    [ts]. *)
val instantiate : scheme -> t

(** Returns a set of free type variables in a given type scheme. *)
val fv_in_scheme : scheme -> VarSet.t

val box_scheme : scheme -> bool * scheme
val unbox_scheme : scheme -> bool * scheme

val pp_scheme : Format.formatter -> scheme -> unit

(** {2 Type declaration} *)

type constr_tag = int [@@deriving show]

type decl =
  | Variant of string (* type name *)
               * type_var list (* type parameters of type constructor *)
               * (constr_tag * string * t list) list (* constructors *)
                 [@@deriving show]

val make_constr_tags : (string * t list) list -> constr_tag list

val constr_scheme : string -> type_var list -> t list -> scheme
