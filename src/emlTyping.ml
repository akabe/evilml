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

module L = EmlLocation
module S = EmlSyntax

type pattern = pattern_desc typed [@@deriving show]
and pattern_desc =
  | Pvar of string option
  | Pconst of S.const_pattern
  | Ptuple of pattern list
  | Pconstr of EmlType.constr_tag * string * pattern list

type expr = ext_expr base_expr
and ext_expr =
  | Match of expr * (pattern * expr) list
  | Constraint of expr * EmlType.t
                    [@@deriving show]

type top = ext_expr EmlTypedExpr.base_top [@@deriving show]

let check_dup ~loc l =
  match List.duplicated l with
  | [] -> ()
  | dups -> errorf ~loc "Duplicated identifier(s): %a"
              (pp_list_comma pp_print_string) dups ()

let check_dup_args ~loc args =
  check_dup ~loc (List.filter_map identity args)

let typing_constr ~loc mk_tuple ctx id args =
  let (_, u_args, u_ret) = EmlContext.lookup_constr ~loc id ctx in
  let unify locs t_args =
    let t_ret = EmlType.fresh_var () in
    List.iter3 (fun loc u t -> EmlType.unify ~loc u t) locs u_args t_args;
    EmlType.unify ~loc u_ret t_ret;
    t_ret
  in
  let t_args = List.map (fun x -> x.typ) args in
  let m = List.length t_args in
  let n = List.length u_args in
  if m = n then (unify (List.map (fun x -> x.loc) args) t_args, args)
  else if n = 1 && m > 1
  then begin
    let typ = EmlType.Tuple t_args in
    (unify [loc] [typ], [{ loc; typ; data = mk_tuple args; }])
  end
  else errorf ~loc "Constructor %s expects %d argument(s) \
                    but %d argument(s) are applied" id n m ()

let rec typing_expr ctx { L.loc; L.data } = match data with
  | S.Error -> mk_exp_error ~loc ()
  | S.Const S.Unit -> mk_exp_unit ~loc ()
  | S.Const (S.Bool b) -> mk_exp_bool ~loc b
  | S.Const (S.Char c) -> mk_exp_char ~loc c
  | S.Const (S.Int n) -> mk_exp_int ~loc n
  | S.Const (S.Float x) -> mk_exp_float ~loc x
  | S.Var s -> mk_exp_var_lookup ~loc ctx s
  | S.Constr (id, el) ->
    let el' = List.map (typing_expr ctx) el in
    let (t_ret, el') = typing_constr ~loc (fun el -> Tuple el) ctx id el' in
    mk_exp_constr ~loc id t_ret el'
  | S.Tuple el -> mk_exp_tuple ~loc (List.map (typing_expr ctx) el)
  | S.EmlOp op -> mk_exp_op ~loc (EmlOp.map (typing_expr ctx) op)
  | S.If (e1, e2, e3) ->
    mk_exp_if ~loc (typing_expr ctx e1)
      (typing_expr ctx e2) (typing_expr ctx e3)
  | S.App (e_fun, e_args) ->
    mk_exp_app ~loc (typing_expr ctx e_fun) (List.map (typing_expr ctx) e_args)
  | S.Abs (args, e_body) ->
    check_dup_args ~loc args;
    mk_exp_abs ~loc ctx typing_expr args e_body
  | S.Let (rf, id, e1, e2) -> mk_exp_let ~loc ctx typing_expr rf id e1 e2
  | S.Constraint (e, t) ->
    let e' = typing_expr ctx e in
    EmlType.unify ~loc e'.typ t;
    { loc; typ = t; data = Ext (Constraint (e', t)); }
  | S.Match (e0, cases) -> typing_match ~loc ctx e0 cases

and typing_match ~loc ctx e0 cases =
  let e0' = typing_expr ctx e0 in
  let (t_in, t_out) = (e0'.typ, EmlType.fresh_var ()) in
  let typing_case (pi, ei) =
    let (ctx', pi') = typing_pattern ctx pi in
    let ei' = typing_expr ctx' ei in
    EmlType.unify ~loc pi'.typ t_in;
    EmlType.unify ~loc ei'.typ t_out;
    (pi', ei')
  in
  let cases' = List.map typing_case cases in
  { loc; typ = t_out; data = Ext (Match (e0', cases')); }

and typing_pattern ctx { L.loc; L.data } = match data with
  | S.Pconst S.Punit ->
    (ctx, { loc; typ = EmlType.Unit; data = Pconst S.Punit; })
  | S.Pconst (S.Pbool b) ->
    (ctx, { loc; typ = EmlType.Unit; data = Pconst (S.Pbool b); })
  | S.Pconst (S.Pchar c) ->
    (ctx, { loc; typ = EmlType.Char; data = Pconst (S.Pchar c); })
  | S.Pconst (S.Pint n) ->
    (ctx, { loc; typ = EmlType.Int; data = Pconst (S.Pint n); })
  | S.Pvar None ->
    (ctx, { loc; typ = EmlType.fresh_var (); data = Pvar None; })
  | S.Pvar (Some id) ->
    let typ = EmlType.fresh_var () in
    (EmlContext.add_var id (EmlType.scheme typ) ctx,
     { loc; typ; data = Pvar (Some id); })
  | S.Ptuple pl ->
    let (ctx', pl') = List.fold_map typing_pattern ctx pl in
    let typ = EmlType.Tuple (List.map (fun p -> p.typ) pl') in
    (ctx', { loc; typ; data = Ptuple pl'; })
  | S.Pconstr (id, pl) ->
    let (ctx', pl') = List.fold_map typing_pattern ctx pl in
    let (t_ret, pl') = typing_constr ~loc (fun pl -> Ptuple pl) ctx id pl' in
    let (tag, _, _) = EmlContext.lookup_constr ~loc id ctx in
    (ctx', { loc; typ = t_ret; data = Pconstr (tag, id, pl'); })

(** Scope analysis for type variables in a given type *)
let convert_type ~loc ctx tbl =
  let rec aux t = match EmlType.observe t with
    | EmlType.Ref _ -> assert false
    | EmlType.Unit | EmlType.Bool | EmlType.Char | EmlType.Int
    | EmlType.Float -> t
    | EmlType.Var (None, _) -> t
    | EmlType.Var (Some s, _) ->
      if Hashtbl.mem tbl s then EmlType.Var (Some s, Hashtbl.find tbl s)
      else errorf ~loc "Unbound type parameter %s" s ()
    | EmlType.Arrow (args, ret) -> EmlType.Arrow (List.map aux args, aux ret)
    | EmlType.Tuple tl -> EmlType.Tuple (List.map aux tl)
    | EmlType.Tconstr (id, tl) ->
      match EmlContext.lookup_type ~loc id ctx with
      | EmlType.Variant (name, args, _) ->
        let m, n = List.length tl, List.length args in
        if m = n then EmlType.Tconstr (id, List.map aux tl)
        else errorf ~loc "The type constructor %s expects %d argument(s), \
                          but is here applied to %d argument(s)" name n m ()
  in
  List.map aux

let convert_variant_type ~loc ctx name args constrs =
  check_dup_args ~loc args;
  check_dup ~loc (List.map fst constrs);
  let tbl = Hashtbl.create 4 in
  let add_to_tbl = function
    | None -> EmlType.fresh_type_var ()
    | Some s ->
      let i = EmlType.fresh_type_var () in
      Hashtbl.add tbl s i ; i
  in
  let args' = List.map add_to_tbl args in
  let ctx' = EmlContext.add_type (* a dummy context for checking rec types *)
      (EmlType.Variant (name, args', [])) ctx in
  let constrs' = List.map2
      (fun tag (name, t) -> (tag, name, convert_type ~loc ctx' tbl t))
      (EmlType.make_constr_tags constrs) constrs in
  EmlType.Variant (name, args', constrs')

let typing ctx =
  let aux ctx { L.loc; L.data } = match data with
    | S.Top_variant_type (name, args, constrs) ->
      let decl = convert_variant_type ~loc ctx name args constrs in
      let ctx' = EmlContext.add_type decl ctx in
      (ctx', { L.loc; L.data = Top_type decl })
    | S.Top_let (rf, id, e1) ->
      let (ts, e1') = mk_exp_let_rhs ~loc ctx typing_expr rf id e1 in
      (EmlContext.add_var id ts ctx,
       { L.loc; L.data = Top_let (rf, id, ts, e1') })
    | S.Top_code s -> (ctx, { L.loc; L.data = Top_code s; })
    | S.Top_use _ -> failwith "Typing.typing: Syntax.Top_use remains"
  in
  List.fold_map aux ctx >> snd
