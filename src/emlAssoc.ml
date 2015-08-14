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
open EmlTypedExpr
open EmlRemoveMatch

let fresh_then_name = gen_fresh_name "__ml_then"
let fresh_else_name = gen_fresh_name "__ml_else"

(** [let_wrap id e] returns expression [let id = e in id]. *)
let let_wrap id e =
  mk_exp_var ~loc:e.loc id e.typ
  |> mk_exp_simple_let ~loc:e.loc false id e

(** [lazy_wrap id e] wraps expression [e] as [let id = e in id]. *)
let lazy_wrap id e =
  let e_abs = { loc = e.loc; typ = EmlType.Arrow ([EmlType.Unit], e.typ);
                data = Abs ([None], e); } in
  let_wrap id e_abs

(** [if e1 then e2 else e3] is converted into
    [(if e1
      then (let __ml_then _ = e2 in __then)
      else (let __ml_else _ = e3 in __else)) ()]
    because C++ template if is implemented as a function (call-by-value). *)
let conv_if ~loc e1 e2 e3 =
  let e2' = lazy_wrap (fresh_then_name ()) e2 in
  let e3' = lazy_wrap (fresh_else_name ()) e3 in
  let e_if = mk_exp_if ~loc e1 e2' e3' in
  mk_exp_app ~loc e_if [mk_exp_unit ~loc ()]

let rec conv_expr e = match e.data with
  | Const _ | Error | Var _ -> e
  | Ext (Tag e0) -> { e with data = Ext (Tag (conv_expr e0)) }
  | Ext (Proj (e0, i)) -> { e with data = Ext (Proj (conv_expr e0, i)) }
  | Tuple el -> { e with data = Tuple (List.map conv_expr el) }
  | Op op -> { e with data = Op (EmlOp.map conv_expr op) }
  | Abs (args, e0) -> { e with data = Abs (args, conv_expr e0) }
  | App (e0, el) ->
    { e with data = App (conv_expr e0, List.map conv_expr el) }
  | Constr (id, el) ->
    { e with data = Constr (id, List.map conv_expr el) }
  | If (e1, e2, e3) ->
    conv_if ~loc:e.loc (conv_expr e1) (conv_expr e2) (conv_expr e3)
  | Let (rf, id, ts, e1, e2) ->
    { e with data = Let (rf, id, ts, conv_expr e1, conv_expr e2) }

let convert = map conv_expr
(*
let rec conv_expr tbl e = match e.data with
  | Const _ | Ext Match_error -> e
  | Var x ->
    (try let f = List.assoc x tbl in f ~loc:e.loc
     with Not_found -> e)
  | Ext (Tag e0) -> { e with data = Ext (Tag (conv_expr tbl e0)) }
  | Ext (Proj (e0, i)) -> { e with data = Ext (Proj (conv_expr tbl e0, i)) }
  | Tuple el -> { e with data = Tuple (List.map (conv_expr tbl) el) }
  | EmlOp op -> { e with data = EmlOp (EmlOp.map (conv_expr tbl) op) }
  | Abs (args, e0) -> { e with data = Abs (args, conv_expr tbl e0) }
  | App (e0, el) ->
    { e with data = App (conv_expr tbl e0,
                         List.map (conv_expr tbl >> conv_arg) el) }
  | Constr (id, el) ->
    { e with data = Constr (id, List.map (conv_expr tbl >> conv_arg) el) }
  | If (e1, e2, e3) ->
    conv_if ~loc:e.loc (conv_expr tbl e1) (conv_expr tbl e2) (conv_expr tbl e3)
  | Let (rf, id, ts, e1, e2) ->
    if rf
    then { e with data = Let (false, id, ts, conv_let_rec_rhs tbl id e1,
                              conv_expr tbl e2) }
    else { e with data = Let (rf, id, ts, conv_expr tbl e1, conv_expr tbl e2) }

(** [let rec id1 = e1] |-> [let rec id2 n = [id1 |-> id2 (n+1)]e1 in id2 0] *)
and conv_let_rec_rhs tbl id1 e1 =
  let cnt = "__cnt" in
  let id2 = "__rec_" ^ id1 in
  let t_fun = EmlType.Arrow ([EmlType.Int], e1.typ) in
  let e_id2 = mk_exp_var ~loc:e1.loc id2 t_fun in
  let e1' = conv_let_rec_subst tbl id1 e_id2 cnt e1 in
  let e_abs = { loc = e1.loc; typ = t_fun;
                data = Abs ([Some cnt], e1'); } in (* fun __cnt -> e1' *)
  let e_app = mk_exp_app ~loc:e1.loc (mk_exp_var ~loc:e1.loc id2 t_fun)
      [mk_exp_int ~loc:e1.loc 0] in (* __rec_id 0 *)
  mk_exp_simple_let ~loc:e1.loc true id2 e_abs e_app

(** [conv_let_rec_subst tbl id1 id2 cnt e] substitutes all occurence of [id1] in
    expression [e] with [id2 (cnt + 1)]. *)
and conv_let_rec_subst tbl id1 id2 cnt e =
  let mk_subst ~loc = (* Generate expr [id2 (cnt + 1)] *)
    let e_vcnt = mk_exp_var ~loc cnt EmlType.Int in
    let e_1 = mk_exp_int ~loc 1 in
    let e_sub = { loc; typ = EmlType.Int; data = EmlOp (EmlOp.Add (e_vcnt, e_1)); } in
    mk_exp_app ~loc { id2 with loc } [e_sub]
  in
  let tbl' = (id1, mk_subst) :: tbl in
  conv_expr tbl' e

let convert =
  let aux = function
    | Top_variant_type (name, args, cs) -> Top_variant_type (name, args, cs)
    | Top_let (rf, id, ts, e) ->
      if rf then Top_let (false, id, ts, conv_let_rec_rhs [] id e)
      else Top_let (rf, id, ts, conv_expr [] e)
  in
  List.map (L.map aux)
  *)
