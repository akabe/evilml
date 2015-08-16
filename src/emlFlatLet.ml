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

module L = EmlLocation
module E = EmlTypedExpr
module B = EmlBoxing

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
  | Top_variant_type of string * EmlType.t list * (int * string * EmlType.t list) list
  | Top_let of let_expr_desc
  | Top_code of string [@@deriving show]

let fresh_fun_name = gen_fresh_name "__ml_fun"

let rec conv_expr rev_lets { E.data; E.typ; _ } = match data with
  | E.Error -> (rev_lets, { typ; data = Error })
  | E.Const c -> (rev_lets, { typ; data = Const c })
  | E.Var id -> (rev_lets, { typ; data = Var id })
  | E.Op op ->
    let (rev_lets', op') = EmlOp.fold_map conv_expr rev_lets op in
    (rev_lets', { typ; data = Op op' })
  | E.Ext (B.Box e1) ->
    let (rev_lets', e1') = conv_expr rev_lets e1 in
    (rev_lets', { typ; data = Box e1'; })
  | E.Ext (B.Unbox e1) ->
    let (rev_lets', e1') = conv_expr rev_lets e1 in
    (rev_lets', { typ; data = Unbox e1'; })
  | E.Ext (B.Tag e1) ->
    let (rev_lets', e1') = conv_expr rev_lets e1 in
    (rev_lets', { typ; data = Tag e1'; })
  | E.Ext (B.Proj (e1, n, i)) ->
    let (rev_lets', e1') = conv_expr rev_lets e1 in
    (rev_lets', { typ; data = Proj (e1', n, i); })
  | E.If (e1, e2, e3) ->
    let (rev_lets', e1') = conv_expr rev_lets e1 in
    let (rev_lets', e2') = conv_expr rev_lets' e2 in
    let (rev_lets', e3') = conv_expr rev_lets' e3 in
    (rev_lets', { typ; data = If (e1', e2', e3'); })
  | E.Tuple el ->
    let (rev_lets', el') = List.fold_map conv_expr rev_lets el in
    (rev_lets', { typ; data = Tuple el' })
  | E.Constr (id, el) ->
    let (rev_lets', el') = List.fold_map conv_expr rev_lets el in
    (rev_lets', { typ; data = Constr (id, el') })
  | E.App (e0, el) ->
    let (rev_lets', e0') = conv_expr rev_lets e0 in
    let (rev_lets', el') = List.fold_map conv_expr rev_lets' el in
    (rev_lets', { typ; data = App (e0', el') })
  | E.Abs (args, e1) -> (* Give a name for anonymous functions *)
    let id = fresh_fun_name () in
    let led = conv_abs false id (EmlType.scheme typ) args e1 in
    let e2 = { data = Var id; typ; } in
    (led :: rev_lets, e2)
  | E.Let (rf, id, ts, { E.data = E.Abs (args, e11); _ }, e2) ->
    let led = conv_abs rf id ts args e11 in
    conv_expr (led :: rev_lets) e2
  | E.Let (_, id, ts, e1, e2) ->
    let rev_lets' = conv_let_val rev_lets id ts e1 in
    conv_expr rev_lets' e2

and conv_abs rf id ts args e =
  let (rev_lets, e') = conv_expr [] e in
  Let_fun (rf, id, ts, args, (List.rev rev_lets, e'))

and conv_let_val rev_lets id ts e =
  let (rev_lets', e') = conv_expr rev_lets e in
  let led = Let_val (id, ts, e') in
  led :: rev_lets'

let convert tops =
  let aux rev_tops e = match e.L.data with
    | E.Top_code s -> Top_code s :: rev_tops
    | E.Top_variant_type (name, args, cs) ->
      (Top_variant_type (name, args, cs)) :: rev_tops
    | E.Top_let (rf, id, ts, { E.data = E.Abs (args, e11); _ }) ->
      (Top_let (conv_abs rf id ts args e11)) :: rev_tops
    | E.Top_let (_, id, ts, e1) ->
      let rev_tops' = conv_let_val [] id ts e1
                      |> List.map (fun l -> Top_let l) in
      rev_tops' @ rev_tops
  in
  List.fold_left aux [] tops |> List.rev
