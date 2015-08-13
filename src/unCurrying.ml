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
open Utils
open TypedExpr
open RemoveMatch

(* Eta conversion for uncurrying partial application *)
let eta_conv ~loc e_fun e_args1 t_args2 t_ret =
  let ids = List.mapi (fun i _ -> "__x" ^ string_of_int i) t_args2 in
  let e_args2 = List.map2 (mk_exp_var ~loc) ids t_args2 in
  let e_app = { loc; typ = t_ret; data = App (e_fun, e_args1 @ e_args2); } in
  { loc; typ = Type.Arrow (t_args2, t_ret);
    data = Abs (List.map (fun x -> Some x) ids, e_app); }

let rec conv_app ~loc e_fun e_args =
  match Type.unarrow e_fun.typ with
  | None -> errorf ~loc "Arrow type is expected but an expression has type %a"
              Type.pp e_fun.typ ()
  | Some (t_args, t_ret) ->
    let m = List.length e_args in (* # of actual arguments *)
    let n = List.length t_args in (* # of formal arguments *)
    if m = n
    then { loc; typ = t_ret; data = App (e_fun, e_args); }
    else if m > n (* application to returned function (e.g., id id 42) *)
    then begin
      let (e_args', rest) = List.block n e_args in
      conv_app ~loc ({ loc; typ = t_ret; data = App (e_fun, e_args'); }) rest
    end
    else begin (* m < n: partial application *)
      let (_, t_args') = List.block m t_args in
      eta_conv ~loc e_fun e_args t_args' t_ret
    end

let rec conv_expr e = match e.data with
  | Const _ | Var _ | Error -> e
  | Ext (Tag e0) -> { e with data = Ext (Tag (conv_expr e0)) }
  | Ext (Proj (e0, i)) -> { e with data = Ext (Proj (conv_expr e0, i)) }
  | Constr (id, el) -> { e with data = Constr (id, List.map conv_expr el) }
  | Tuple el -> { e with data = Tuple (List.map conv_expr el) }
  | Op op -> { e with data = Op (Op.map conv_expr op) }
  | If (e1, e2, e3) ->
    { e with data = If (conv_expr e1, conv_expr e2, conv_expr e3) }
  | Abs (args, e0) -> { e with data = Abs (args, conv_expr e0) }
  | Let (rf, id, ts, e1, e2) ->
    { e with data = Let (rf, id, ts, conv_expr e1, conv_expr e2) }
  | App (e0, el) -> conv_app ~loc:e.loc (conv_expr e0) (List.map conv_expr el)

let convert = map conv_expr
