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

type elt = string * EmlType.scheme

type t = elt list

let empty = []

let add_var id tysc ctx = (id, tysc) :: ctx

let add_args ids tys ctx =
  List.fold_left2 (fun acc t -> function
      | Some x -> (x, EmlType.scheme t) :: acc
      | None -> acc) ctx tys ids

let lookup ~loc id ctx =
  try List.assoc id ctx
  with Not_found -> errorf ~loc "Unbound identifier `%s'" id ()

let fv_in_context =
  List.fold_left
    (fun acc (_, ts) -> EmlType.VarSet.union acc (EmlType.fv_in_scheme ts))
    EmlType.VarSet.empty

let generalize_type ctx t =
  let bv = fv_in_context ctx in (* Type variables bound in a typing context *)
  let fv = EmlType.fv_in_type t in (* Type variables free in a type *)
  EmlType.generalize (EmlType.VarSet.diff fv bv) t
