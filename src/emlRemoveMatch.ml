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
open EmlTypedExpr
open EmlUtils

module T = EmlTyping

type expr = ext_expr base_expr [@@deriving show]
and ext_expr =
  | Tag of expr (* Obtain the tag of a data constructor *)
  | Proj of expr * int (* Projection operator *)

type top = ext_expr base_top [@@deriving show]

let mk_exp_proj ~loc ~typ e i = { loc; typ; data = Ext (Proj (e, i)); }
let mk_exp_tag ~loc e = { loc; typ = EmlType.Int; data = Ext (Tag e); }
let mk_exp_eq ~loc e1 e2 =
  { loc; typ = EmlType.Bool; data = Op (EmlOp.Eq (e1, e2)); }
let mk_exp_if_eq ~loc e_lhs e_rhs e2 e3 =
  mk_exp_if ~loc (mk_exp_eq ~loc e_lhs e_rhs) e2 e3

let rec conv_pat e e_then e_else p =
  let loc = e.loc in
  match p.data with
  | T.Pvar None | T.Pconst EmlSyntax.Punit -> e_then
  | T.Pvar (Some id) -> (* let id = e in e_then *)
    mk_exp_simple_let ~loc false id e e_then
  | T.Pconst (EmlSyntax.Pbool true) -> (* if e then e_then else e_else *)
    mk_exp_if ~loc e e_then e_else
  | T.Pconst (EmlSyntax.Pbool false) -> (* if e then e_else else e_then *)
    mk_exp_if ~loc e e_else e_then
  | T.Pconst (EmlSyntax.Pchar c) -> (* if e = c then e_then else e_else *)
    mk_exp_if_eq ~loc e (mk_exp_char ~loc c) e_then e_else
  | T.Pconst (EmlSyntax.Pint n) -> (* if e = n then e_then else e_else *)
    mk_exp_if_eq ~loc e (mk_exp_int ~loc n) e_then e_else
  | T.Ptuple pl -> conv_pat_list ~loc e e_then e_else pl
  | T.Pconstr (tag, _, pl) ->
    let e_then' = conv_pat_list ~loc e e_then e_else pl in
    mk_exp_if_eq ~loc (mk_exp_tag ~loc e) (mk_exp_int ~loc tag) e_then' e_else

and conv_pat_list ~loc e e_then e_else pl =
  List.fold_righti
    (fun i pi acc -> conv_pat (mk_exp_proj ~loc ~typ:pi.typ e i) acc e_else pi)
    pl e_then

let rec conv_expr e = match e.data with
  | Error -> { e with data = Error }
  | Const c -> { e with data = Const c }
  | Var id -> { e with data = Var id }
  | Constr (id, el) -> { e with data = Constr (id, List.map conv_expr el) }
  | Tuple el -> { e with data = Tuple (List.map conv_expr el) }
  | Op op -> { e with data = Op (EmlOp.map conv_expr op) }
  | Abs (args, e0) -> { e with data = Abs (args, conv_expr e0) }
  | App (e0, el) ->
    { e with data = App (conv_expr e0, List.map conv_expr el) }
  | If (e1, e2, e3) ->
    { e with data = If (conv_expr e1, conv_expr e2, conv_expr e3) }
  | Let (rf, id, ts, e1, e2) ->
    { e with data = Let (rf, id, ts, conv_expr e1, conv_expr e2) }
  | Ext (T.Constraint (e1, _)) -> conv_expr e1 (* Remove type constraints *)
  | Ext (T.Match (e0, cases)) ->
    let e0' = conv_expr e0 in
    List.fold_right (fun (pi, ei) acc -> conv_pat e0' (conv_expr ei) acc pi)
      cases { e with typ = EmlType.genvar (); data = Error; }

let convert = map conv_expr
