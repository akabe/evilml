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

type elt =
  | Var of string * EmlType.scheme
  | Constr of string * EmlType.constr_tag * EmlType.scheme
  | Type of EmlType.decl

type t = elt list

let empty = []

let add_var id tysc ctx = Var (id, tysc) :: ctx

let add_args ids tys ctx =
  List.fold_left2 (fun acc t -> function
      | Some x -> Var (x, EmlType.scheme t) :: acc
      | None -> acc) ctx tys ids

let add_type (EmlType.Variant (ty_name, ty_vars, constrs) as decl) ctx =
  let add_constr ctx (tag, c_name, c_args) =
    let tysc = EmlType.constr_scheme ty_name ty_vars c_args in
    Constr (c_name, tag, tysc) :: ctx
  in
  List.fold_left add_constr (Type decl :: ctx) constrs

let lookup_var ~loc id ctx =
  let aux = function
    | Var (s, tysc) when id = s -> Some tysc
    | _ -> None
  in
  match List.find_map aux ctx with
  | Some tysc -> tysc
  | None -> errorf ~loc "Unbound variable `%s'" id ()

let lookup_constr ~loc id ctx =
  let aux = function
    | Constr (s, tag, tysc) when id = s ->
      Some (tag, EmlType.unarrow (EmlType.instantiate tysc))
    | _ -> None
  in
  match List.find_map aux ctx with
  | Some (tag, Some (t_args, t_ret)) -> (tag, t_args, t_ret)
  | Some (_, None) -> errorf ~loc "Constructor %s has strange type" id ()
  | None -> errorf ~loc "Unbound constructor `%s'" id ()

let lookup_type ~loc id ctx =
  let aux = function
    | Type (EmlType.Variant (s, _, _) as decl) when id = s -> Some decl
    | _ -> None
  in
  match List.find_map aux ctx with
  | Some decl -> decl
  | None -> errorf ~loc "Unbound type constructor `%s'" id ()

let fv_in_context =
  List.fold_left
    (fun acc -> function
       | Var (_, ts) -> EmlType.VarSet.union acc (EmlType.fv_in_scheme ts)
       | _ -> acc)
    EmlType.VarSet.empty

let generalize_type ctx t =
  let bv = fv_in_context ctx in (* Type variables bound in a typing context *)
  let fv = EmlType.fv_in_type t in (* Type variables free in a type *)
  EmlType.generalize (EmlType.VarSet.diff fv bv) t
