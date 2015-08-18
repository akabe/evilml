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
open Format
open EmlTypedExpr

module M = EmlRemoveMatch

type expr = ext_expr base_expr [@@deriving show]
and ext_expr =
  | Box of expr
  | Unbox of expr
  | Tag of expr (* Obtain the tag of a data constructor *)
  | Proj of expr * int * int (* Projection operator *)

type top = ext_expr base_top [@@deriving show]

(** Insert boxing if a given expression has a base type. *)
let box_expr e =
  let (need, typ) = EmlType.box_type e.typ in
  { loc = e.loc; typ; data = if need then Ext (Box e) else e.data; }

(** Insert unboxing if a given expression has a boxed type. *)
let unbox_expr e = match EmlType.unbox_type e.typ with
  | (true, typ) -> { loc = e.loc; typ; data = Ext (Unbox e); }
  | _ -> e

type expected_type =
  | Nothing
  | Boxed
  | Unboxed

let box_unbox_type tt t = match tt with
  | Nothing -> t
  | Boxed -> snd (EmlType.box_type t)
  | Unboxed -> snd (EmlType.unbox_type t)

let box_unbox_scheme tt ts = match tt with
  | Nothing -> ts
  | Boxed -> snd (EmlType.box_scheme ts)
  | Unboxed -> snd (EmlType.unbox_scheme ts)

let box_unbox_expr tt e = match tt with
  | Nothing -> e
  | Boxed -> box_expr e
  | Unboxed -> unbox_expr e

let rec conv_expr tt ctx e = match e.data with
  | Const c -> box_unbox_expr tt { e with data = Const c }
  | Var id ->
    let typ = (*try EmlType.instantiate (List.assoc id ctx)
                with Not_found -> e.typ*)
      EmlType.instantiate (EmlContext.lookup ~loc:e.loc id ctx) in
    box_unbox_expr tt { e with data = Var id; typ }
  | Error -> (* box/unbox is not needed since Error has forall 'a. 'a. *)
    { e with data = Error; typ = box_unbox_type tt e.typ; }
  | Ext (M.Tag e0) ->
    box_unbox_expr tt { e with data = Ext (Tag (conv_expr Nothing ctx e0)) }
  | Let (rf, id, ts, e1, e2) ->
    let (ts', e1') = conv_let_rhs ctx rf id ts e1 in
    let e2' = conv_expr tt (EmlContext.add_var id ts' ctx) e2 in
    { e with data = Let (rf, id, ts', e1', e2'); typ = e2'.typ }
  (* Constructors: all arguments are boxed. *)
  | Constr (id, el) ->
    { e with typ = box_unbox_type tt e.typ;
             data = Constr (id, List.map (conv_expr Boxed ctx) el) }
  (* Projection: obtained elements are boxed. *)
  | Ext (M.Proj (e0, n, i)) ->
    let e0' = conv_expr Nothing ctx e0 in
    let (_, typ) = EmlType.box_type e.typ in
    { e with typ; data = Ext (Proj (e0', n, i)) }
    |> box_unbox_expr tt
  (* Tuples: elements of tuples are boxed. *)
  | Tuple el -> mk_exp_tuple ~loc:e.loc (List.map (conv_expr Boxed ctx) el)
  (* Operators: all arguments and a return value are unboxed. *)
  | Op op ->
    let op' = EmlOp.map (conv_expr Unboxed ctx) op in
    box_unbox_expr tt (mk_exp_op ~loc:e.loc op')
  (* If: the 1st argument is unboxed, others are boxed. *)
  | If (e1, e2, e3) ->
    mk_exp_if ~loc:e.loc (conv_expr Unboxed ctx e1)
      (conv_expr Boxed ctx e2) (conv_expr Boxed ctx e3)
    |> box_unbox_expr tt
  (* Functions: all arguments and return values of functions are boxed. *)
  | Abs (args, e0) ->
    let t_args = match EmlType.unarrow e.typ with
      | None -> assert false
      | Some (t_args, _) -> List.map (EmlType.box_type >> snd) t_args in
    let ctx' = EmlContext.add_args args t_args ctx in
    let e0' = conv_expr Boxed ctx' e0 in
    let t_fun = EmlType.Arrow (t_args, e0'.typ) in
    { loc = e.loc; typ = t_fun; data = Abs (args, e0'); }
  | App (e0, el) ->
    let e0' = conv_expr Boxed ctx e0 in
    let el' = List.map (conv_expr Boxed ctx) el in
    box_unbox_expr tt (mk_exp_app ~loc:e.loc e0' el')

and conv_let_rhs ctx rf id ts e =
  let ts' = box_unbox_scheme Boxed ts in
  let ctx' = if rf then EmlContext.add_var id ts' ctx else ctx in
  let e' = conv_expr Boxed ctx' e in
  (ts', e')

let conv_constr (tag, id, args) =
  (tag, id, List.map (EmlType.box_type >> snd) args)

let convert ctx tops =
  let f_vtype _ ctx name args constrs =
    let constrs' = List.map conv_constr constrs in
    let tss = typeof_constrs name args constrs' in
    let ctx' = List.fold_left2
        (fun acc (_, id, _) ts -> EmlContext.add_var id ts ctx)
        ctx constrs' tss in
    (ctx', Top_variant_type (name, args, constrs')) (* TODO *)
  in
  let f_let _ ctx rf id ts e =
    let (ts', e') = conv_let_rhs ctx rf id ts e in
    (EmlContext.add_var id ts' ctx, Top_let (rf, id, ts', e'))
  in
  snd (fold_map f_vtype f_let ctx tops)
