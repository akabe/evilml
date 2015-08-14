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
open TypedExpr
open Utils

module M = RemoveMatch

type expr = ext_expr base_expr [@@deriving show]
and ext_expr =
  | Box of expr
  | Unbox of expr
  | Tag of expr (* Obtain the tag of a data constructor *)
  | Proj of expr * int (* Projection operator *)

type top = ext_expr base_top [@@deriving show]

let rec box_type t = match Type.observe t with
  | Type.Bool | Type.Char | Type.Int | Type.Float ->
    Type.Tconstr ("__ml_boxed", [t])
  | Type.Arrow (args, ret) ->
    Type.Arrow (List.map box_type args, box_type ret)
  | Type.Tconstr (name, tl) when name <> "__ml_boxed" ->
    Type.Tconstr (name, List.map box_type tl)
  | _ -> t

let unbox_type t = match Type.observe t with
  | Type.Tconstr ("__ml_boxed", [t']) -> Some t'
  | _ -> None

(** Insert boxing if a given expression has a base type. *)
let box_expr e =
  { loc = e.loc;
    typ = box_type e.typ;
    data = if Type.is_basetype e.typ then Ext (Box e) else e.data; }

(** Insert unboxing if a given expression has a boxed type. *)
let unbox_expr e = match unbox_type e.typ with
  | Some t -> { loc = e.loc; typ = t; data = Ext (Unbox e); }
  | None -> e

let rec conv_expr ctx { loc; typ; data; } = match data with
  | Const c -> { loc; typ; data = Const c; }
  | Var id -> mk_exp_var_lookup ~loc ctx id
  | Error -> mk_exp_error ~loc ()
  | Ext (M.Tag e0) -> { loc; typ; data = Ext (Tag (conv_expr ctx e0)) }
  | Let (rf, id, _, e1, e2) -> mk_exp_let ~loc ctx conv_let_expr rf id e1 e2
  (* Constructors: all arguments are boxed. *)
  | Constr (id, el) -> List.map (conv_box_expr ctx) el
                       |> mk_exp_constr_lookup ~loc ctx id
  (* Projection: obtained elements are boxed. *)
  | Ext (M.Proj (e0, i)) ->
    let e0' = conv_expr ctx e0 in
    { loc; typ = box_type typ; data = Ext (Proj (e0', i)) }
  (* Tuples: elements of tuples are boxed. *)
  | Tuple el -> mk_exp_tuple ~loc (List.map (conv_box_expr ctx) el)
  (* Operators: all arguments and a return value are unboxed. *)
  | Op op -> { loc; typ = Option.default typ (unbox_type typ);
               data = Op (Op.map (conv_unbox_expr ctx) op); }
  (* If: the 1st argument is unboxed, others are boxed. *)
  | If (e1, e2, e3) -> mk_exp_if ~loc (conv_unbox_expr ctx e1)
                         (conv_box_expr ctx e2) (conv_box_expr ctx e3)
  (* Functions: all arguments and return values of functions are boxed. *)
  | Abs (args, e0) ->
    let t_args = match Type.unarrow typ with
      | None -> assert false
      | Some (t_args, _) -> t_args in
    let arg_types = List.map box_type t_args in
    mk_exp_abs ~arg_types ~loc ctx conv_box_expr args e0
  | App (e0, el) ->
    let e0' = conv_box_expr ctx e0 in
    let el' = List.map (conv_box_expr ctx) el in
    mk_exp_app ~loc e0' el'

and conv_box_expr ctx e = e |> conv_expr ctx |> box_expr
and conv_unbox_expr ctx e = e |> conv_expr ctx |> unbox_expr
and conv_let_expr ctx = conv_expr ctx >> box_expr

let conv_constr (tag, id, args) = (tag, id, List.map box_type args)

let convert ctx tops =
  let f_vtype _ ctx name args constrs =
    let constrs' = List.map conv_constr constrs in
    let tss = typeof_constrs name args constrs' in
    let ctx' = List.rev_map2 (fun (_, id, _) ts -> (id, ts)) constrs' tss in
    (ctx' @ ctx, Top_variant_type (name, args, constrs'))
  in
  let f_let loc ctx rf id _ e =
    let (ts, e') = mk_exp_let_rhs ~loc ctx conv_let_expr rf id e in
    ((id, ts) :: ctx, Top_let (rf, id, ts, e'))
  in
  snd (fold_map f_vtype f_let ctx tops)
