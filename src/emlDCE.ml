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
open EmlRemoveMatch

module L = EmlLocation

let rec conv_expr fv e = match e.data with
  | Const _ | Error -> (fv, e)
  | Var id -> (StringSet.add id fv, e)
  | Constr (id, el) ->
    let (fv', el') = List.fold_map conv_expr fv el in
    (StringSet.add id fv', { e with data = Constr (id, el') })
  | Ext (Tag e0) ->
    let (fv', e0') = conv_expr fv e0 in
    (fv', { e with data = Ext (Tag e0') })
  | Ext (Proj (e0, i)) ->
    let (fv', e0') = conv_expr fv e0 in
    (fv', { e with data = Ext (Proj (e0', i)) })
  | Tuple el ->
    let (fv', el') = List.fold_map conv_expr fv el in
    (fv', { e with data = Tuple el' })
  | Op op ->
    let (fv', op') = EmlOp.fold_map conv_expr fv op in
    (fv', { e with data = Op op' })
  | If (e1, e2, e3) ->
    let (fv', e1') = conv_expr fv e1 in
    let (fv', e2') = conv_expr fv' e2 in
    let (fv', e3') = conv_expr fv' e3 in
    (fv', { e with data = If (e1', e2', e3') })
  | App (e0, el) ->
    let (fv', e0') = conv_expr fv e0 in
    let (fv', el') = List.fold_map conv_expr fv' el in
    (fv', { e with data = App (e0', el') })
  | Abs (args, e0) ->
    let bv = StringSet.of_list (List.filter_map identity args) in
    let (fv0, e0') = conv_expr StringSet.empty e0 in
    let fv' = StringSet.union fv (StringSet.diff fv0 bv) in
    (fv', { e with data = Abs (args, e0') })
  | Let (rf, id, ts, e1, e2) ->
    let (fv2, e2') = conv_expr StringSet.empty e2 in
    if StringSet.mem id fv2
    then begin
      let (fv', e1') = conv_expr_let_rhs (StringSet.union fv fv2) id e1 in
      (fv', { e with data = Let (rf, id, ts, e1', e2') })
    end else begin
      eprintf "DCE: local variable `%s' is eliminated.@." id;
      (StringSet.union fv fv2, e2')
    end

and conv_expr_let_rhs fv id e =
  let (fv', e') = conv_expr fv e in
  (StringSet.remove id fv', e')

(** [find_string str sub] finds substring [sub] from [str]. *)
let find_string str sub =
  let ifind f m n = (* Find index [i] that satisfies [f i]. *)
    let rec aux i = if i > n then None else if f i then Some i else aux (i+1) in
    aux m
  in
  let strlen = String.length str in
  let sublen = String.length sub in
  ifind (fun i -> None = ifind (fun j -> sub.[j] <> str.[i+j]) 0 (sublen - 1))
    0 (strlen - sublen)

let check_word_boundary s i =
  match String.get s i with
  | exception _ -> true
  | c -> not (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_')

let find_word str sub =
  match find_string str sub with
  | None -> false
  | Some bp -> check_word_boundary str (bp - 1) &&
               check_word_boundary str (bp + String.length sub)

(** [check_top_ident fv codes id] checks whether top-level identifier [id] is
    used after its definition, or not. *)
let check_top_ident fv codes id =
  StringSet.mem id fv || List.exists (fun s -> find_word s id) codes

let check_variant_type fv codes constrs =
  List.fold_left
    (fun (used, fv) (_, id, _) ->
       if check_top_ident fv codes id
       then (true, StringSet.remove id fv)
       else (used, fv))
    (false, fv) constrs

let convert tops0 =
  let aux top (fv, codes, tops) = match top.L.data with
    | Top_code s -> (fv, s :: codes, top :: tops)
    | Top_variant_type (name, _, constrs) ->
      begin
        match check_variant_type fv codes constrs with
        | (true, fv') -> (fv', codes, top :: tops)
        | (false, _) ->
          eprintf "DCE: type (constructor) `%s' is eliminated.@." name;
          (fv, codes, tops)
      end
    | Top_let (rf, id, ts, e) ->
      if check_top_ident fv codes id
      then begin
        let (fv', e') = conv_expr_let_rhs fv id e in
        (fv', codes, { top with L.data = Top_let (rf, id, ts, e') } :: tops)
      end else begin
        eprintf "DCE: top-level variable `%s' is eliminated.@." id;
        (fv, codes, tops)
      end
  in
  let (_, _, tops') = List.fold_right aux tops0 (StringSet.empty, [], []) in
  tops'
