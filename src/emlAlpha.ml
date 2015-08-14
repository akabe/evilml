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
open EmlBoxing

type renamer = StringSet.t * (string * string) list

let make_renamer tbl0 tops =
  let add ?(loc = EmlLocation.dummy) (seen, tbl) id1 id2 =
    if StringSet.mem id2 seen
    then errorf ~loc "Duplicated identifier %s" id2 ();
    (StringSet.add id2 seen, (id1, id2) :: tbl)
  in
  let aux rnm top = match top.EmlLocation.data with
    | Top_code _ | Top_variant_type _ -> rnm
    | Top_let (_, id, _, _) -> add ~loc:top.EmlLocation.loc rnm id id
  in
  let rnm =
    List.fold_left (fun rnm (id1, id2) -> add rnm id1 id2)
      (StringSet.empty, []) tbl0 in
  List.fold_left aux rnm tops

let genid seen s =
  let rec aux n =
    let s' = s ^ string_of_int n in
    if StringSet.mem s' seen then aux (n+1) else s'
  in
  let s' = if StringSet.mem s seen then aux 1 else s in
  (StringSet.add s' seen, s')

let rename_args tbl = List.map (Option.map (fun x -> List.assoc x tbl))

let rec conv_expr tbl seen e = match e.data with
  | Const _ | Error -> (seen, e)
  | Ext (Tag e0) ->
    let (seen', e0') = conv_expr tbl seen e0 in
    (seen', { e with data = Ext (Tag e0') })
  | Ext (Proj (e0, i)) ->
    let (seen', e0') = conv_expr tbl seen e0 in
    (seen', { e with data = Ext (Proj (e0', i)) })
  | Ext (Box e0) ->
    let (seen', e0') = conv_expr tbl seen e0 in
    (seen', { e with data = Ext (Box e0') })
  | Ext (Unbox e0) ->
    let (seen', e0') = conv_expr tbl seen e0 in
    (seen', { e with data = Ext (Unbox e0') })
  | Var s -> (seen, { e with data = Var (List.assoc s tbl) })
  | Constr (s, el) ->
    let (seen', el') = List.fold_map (conv_expr tbl) seen el in
    (seen', { e with data = Constr (s, el') })
  | Tuple el ->
    let (seen', el') = List.fold_map (conv_expr tbl) seen el in
    (seen', { e with data = Tuple el' })
  | Op op ->
    let (seen', op') = EmlOp.fold_map (conv_expr tbl) seen op in
    (seen', { e with data = Op op' })
  | If (e1, e2, e3) ->
    let (seen', e1') = conv_expr tbl seen e1 in
    let (seen', e2') = conv_expr tbl seen' e2 in
    let (seen', e3') = conv_expr tbl seen' e3 in
    (seen', { e with data = If (e1', e2', e3') })
  | App (e0, el) ->
    let (seen', e0') = conv_expr tbl seen e0 in
    let (seen', el') = List.fold_map (conv_expr tbl) seen' el in
    (seen', { e with data = App (e0', el') })
  | Abs (args, e0) ->
    let org_args = List.filter_map (fun x -> x) args in
    let (seen', new_args) = List.fold_map genid seen org_args in
    let tbl' = List.combine org_args new_args @ tbl in
    let (seen', e0') = conv_expr tbl' seen' e0 in
    (seen', { e with data = Abs (rename_args tbl' args, e0') })
  | Let (rf, id, ts, e1, e2) ->
    let (seen', new_id) = genid seen id in
    let tbl' = (id, new_id) :: tbl in
    let (seen', e1') = if rf then conv_expr tbl' seen' e1
      else conv_expr tbl seen e1 in
    let (seen', e2') = conv_expr tbl' seen' e2 in
    (seen', { e with data = Let (rf, new_id, ts, e1', e2') })

let convert (seen, tbl) tops =
  let aux = function (* top-level identifiers will not be renamed. *)
    | Top_code _ | Top_variant_type _ as e -> e
    | Top_let(rf, id, ts, e) -> Top_let(rf, id, ts, snd (conv_expr tbl seen e))
  in
  List.map (EmlLocation.map aux) tops
