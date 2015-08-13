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

module L = Location
module S = Syntax

type pattern = pattern_desc typed [@@deriving show]
and pattern_desc =
  | Pvar of string option
  | Pconst of S.const_pattern
  | Ptuple of pattern list
  | Pconstr of constr_tag * string * pattern list

type expr = ext_expr base_expr
and ext_expr =
  | Match of expr * (pattern * expr) list
  | Constraint of expr * Type.t
                    [@@deriving show]

type top = ext_expr TypedExpr.base_top [@@deriving show]

let mk_exp_op ~loc op =
  let typ = match op with
    (* comparison operators (polymorphic) *)
    | Op.Eq (e1, e2) | Op.Ne (e1, e2) | Op.Gt (e1, e2) | Op.Lt (e1, e2)
    | Op.Ge (e1, e2) | Op.Le (e1, e2) ->
      Type.unify ~loc e1.typ e2.typ;
      Type.Bool
    (* boolean operators *)
    | Op.Not e1 ->
      Type.unify ~loc e1.typ Type.Bool;
      Type.Bool
    | Op.And (e1, e2) | Op.Or (e1, e2) ->
      Type.unify ~loc e1.typ Type.Bool;
      Type.unify ~loc e2.typ Type.Bool;
      Type.Bool
    (* integer operators *)
    | Op.Pos e1 | Op.Neg e1 ->
      Type.unify ~loc e1.typ Type.Int;
      Type.Int
    | Op.Add (e1, e2) | Op.Sub (e1, e2) | Op.Mul (e1, e2) | Op.Div (e1, e2)
    | Op.Mod (e1, e2) ->
      Type.unify ~loc e1.typ Type.Int;
      Type.unify ~loc e2.typ Type.Int;
      Type.Int
    (* floating-point-value operators *)
    | Op.FPos e1 | Op.FNeg e1 ->
      Type.unify ~loc e1.typ Type.Float;
      Type.Int
    | Op.FAdd (e1, e2) | Op.FSub (e1, e2) | Op.FMul (e1, e2)
    | Op.FDiv (e1, e2) ->
      Type.unify ~loc e1.typ Type.Float;
      Type.unify ~loc e2.typ Type.Float;
      Type.Float
  in
  { loc; typ; data = Op op; }

let rec typing_expr ctx { L.loc; L.data } = match data with
  | S.Error -> mk_exp_error ~loc ()
  | S.Const S.Unit -> mk_exp_unit ~loc ()
  | S.Const (S.Bool b) -> mk_exp_bool ~loc b
  | S.Const (S.Char c) -> mk_exp_char ~loc c
  | S.Const (S.Int n) -> mk_exp_int ~loc n
  | S.Const (S.Float x) -> mk_exp_float ~loc x
  | S.Var s -> mk_exp_var_lookup ~loc ctx s
  | S.Constr (s, el) -> List.map (typing_expr ctx) el
                        |> mk_exp_constr_lookup ~loc ctx s
  | S.Tuple el -> mk_exp_tuple ~loc (List.map (typing_expr ctx) el)
  | S.Op op -> mk_exp_op ~loc (Op.map (typing_expr ctx) op)
  | S.If (e1, e2, e3) ->
    mk_exp_if ~loc
      (typing_expr ctx e1) (typing_expr ctx e2) (typing_expr ctx e3)
  | S.App (e_fun, e_args) ->
    mk_exp_app ~loc (typing_expr ctx e_fun) (List.map (typing_expr ctx) e_args)
  | S.Abs (args, e_body) -> mk_exp_abs ~loc ctx typing_expr args e_body
  | S.Let (rf, id, e1, e2) -> mk_exp_let ~loc ctx typing_expr rf id e1 e2
  | S.Constraint (e, t) ->
    let e' = typing_expr ctx e in
    Type.unify ~loc e'.typ t;
    { loc; typ = t; data = Ext (Constraint (e', t)); }
  | S.Match (e0, cases) -> typing_match ~loc ctx e0 cases

and typing_match ~loc ctx e0 cases =
  let e0' = typing_expr ctx e0 in
  let (t_in, t_out) = (e0'.typ, Type.genvar ()) in
  let typing_case (pi, ei) =
    let (ctx', pi') = typing_pattern ctx pi in
    let ei' = typing_expr ctx' ei in
    Type.unify ~loc pi'.typ t_in;
    Type.unify ~loc ei'.typ t_out;
    (pi', ei')
  in
  let cases' = List.map typing_case cases in
  { loc; typ = t_out; data = Ext (Match (e0', cases')); }

and typing_pattern ctx { L.loc; L.data } = match data with
  | S.Pconst S.Punit ->
    (ctx, { loc; typ = Type.Unit; data = Pconst S.Punit; })
  | S.Pconst (S.Pbool b) ->
    (ctx, { loc; typ = Type.Unit; data = Pconst (S.Pbool b); })
  | S.Pconst (S.Pchar c) ->
    (ctx, { loc; typ = Type.Char; data = Pconst (S.Pchar c); })
  | S.Pconst (S.Pint n) ->
    (ctx, { loc; typ = Type.Int; data = Pconst (S.Pint n); })
  | S.Pvar None ->
    (ctx, { loc; typ = Type.genvar (); data = Pvar None; })
  | S.Pvar (Some id) ->
    let typ = Type.genvar () in
    ((id, Type.scheme typ) :: ctx, { loc; typ; data = Pvar (Some id); })
  | S.Ptuple pl ->
    let (ctx', pl') = List.fold_map typing_pattern ctx pl in
    let typ = Type.Tuple (List.map (fun p -> p.typ) pl') in
    (ctx', { loc; typ; data = Ptuple pl'; })
  | S.Pconstr (id, pl) ->
    let (ctx', pl') = List.fold_map typing_pattern ctx pl in
    let t_pat = Type.genvar () in
    let t_constr = Type.Arrow (List.map (fun p -> p.typ) pl', t_pat) in
    let ts = Type.lookup ~loc id ctx in
    Type.unify ~loc t_constr (Type.instantiate ts);
    (ctx', { loc; typ = t_pat; data = Pconstr (Hashtbl.hash id, id, pl'); })

let typing ctx =
  let aux ctx { L.loc; L.data } = match data with
    | S.Top_variant_type (name, targs, cs) ->
      let cs' = List.map (fun (id, cargs) -> (Hashtbl.hash id, id, cargs)) cs in
      let tss = typeof_constrs name targs cs' in
      let ctx' = List.map2 (fun (id, _) ts -> (id, ts)) cs tss in
      (ctx' @ ctx, { L.loc; L.data = Top_variant_type (name, targs, cs') })
    | S.Top_let (rf, id, e1) ->
      let (ts, e1') = mk_exp_let_rhs ~loc ctx typing_expr rf id e1 in
      ((id, ts) :: ctx, { L.loc; L.data = Top_let (rf, id, ts, e1') })
    | S.Top_code s -> (ctx, { L.loc; L.data = Top_code s; })
  in
  List.fold_map aux ctx >> snd
