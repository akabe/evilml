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

module F = EmlFlatLet

type template_flag = bool
type typename_flag = bool

type expr =
  {
    dep : bool; (* true if an expr is dependent on template parameters *)
    data : expr_desc;
  }
and expr_desc =
  | Error
  | Const of EmlSyntax.const
  | Var of string
  | Op of expr EmlOp.t
  | App of expr * type_expr list
  | Tmember of template_flag * expr * string
  | Vmember of expr * string

and type_expr = typename_flag * expr

type decl =
  | Code of string
  | Static of string * EmlType.t * expr
  | EmlTypedef of string * type_expr
  | Class of string * decl list * decl list (* private/public *)
  | Template_class of string * string option list * decl list * decl list

(** {2 Conversion} *)

let member_fst = "fst"
let member_snd = "snd"
let member_tag = "tag"
let member_val = "val"
let member_fun = "fun"
let member_ret = "type"

let get_constr_name = function
  | "[]" -> "__ml_nil"
  | "::" -> "__ml_cons"
  | id -> id

let mk_type_expr e : type_expr = match e.data with
  | Tmember (_, { dep = true; _ }, _) -> (true, e)
  | _ -> (false, e)

let mk_exp_const c = { dep = false; data = Const c; }
let mk_exp_var ?(deps = []) id = { dep = List.mem id deps; data = Var id; }
let mk_exp_op op = { dep = EmlOp.exists (fun ei -> ei.dep) op; data = Op op; }
let mk_exp_vmem e mem = { dep = e.dep; data = Vmember (e, mem); }
let mk_exp_tmem ?(deps = []) ?(template = false) e mem =
  let flag = template && not (List.is_empty deps) in
  { dep = e.dep; data = Tmember (flag, e, mem); }

let mk_exp_app e0 el =
  let dep = e0.dep || List.exists (fun ei -> ei.dep) el in
  { dep; data = App (e0, List.map mk_type_expr el); }

let mk_exp_box ~typ e =
  mk_exp_app (mk_exp_var (sfprintf "__ml_%a" EmlType.pp typ ())) [e]

let mk_exp_unbox e = mk_exp_vmem e member_val
let mk_exp_pair e1 e2 = mk_exp_app (mk_exp_var "__ml_pair") [e1; e2]

let mk_exp_tuple el = match List.rev el with
  | [] -> assert false
  | last :: rest -> List.fold_left (fun acc ei -> mk_exp_pair ei acc) last rest

let rec mk_exp_proj e n i = match n, i with
  | _, 0 -> mk_exp_tmem e member_fst
  | 2, 1 -> mk_exp_tmem e member_snd
  | _, i -> mk_exp_proj (mk_exp_tmem e member_snd) (n - 1) (i - 1)

let mk_decl_typedef id e = EmlTypedef (id, mk_type_expr e)
let mk_decl_tag tag =
  Static (member_tag, EmlType.Int, mk_exp_const (EmlSyntax.Int tag))
let mk_decl_ret e = [mk_decl_typedef member_ret e]

(** [deps] is a list of variable names dependent on template parameters. *)
let rec conv_expr deps { F.data; _ } = match data with
  | F.Error -> { dep = false; data = Error; }
  | F.Const c -> mk_exp_const c
  | F.Var id -> mk_exp_var ~deps id
  | F.If (e1, e2, e3) ->
    mk_exp_tmem
      (mk_exp_app
         (mk_exp_var "__ml_if")
         [conv_expr deps e1; conv_expr deps e2; conv_expr deps e3])
      member_ret
  | F.Op op -> conv_op deps op
  | F.Tuple el -> mk_exp_tuple (List.map (conv_expr deps) el)
  | F.Constr (id, []) -> mk_exp_var (get_constr_name id)
  | F.Constr (id, el) ->
    mk_exp_app (mk_exp_var (get_constr_name id)) (List.map (conv_expr deps) el)
  | F.App (e0, el) ->
    mk_exp_tmem
      (mk_exp_app
         (mk_exp_tmem ~deps ~template:true (conv_expr deps e0) member_fun)
         (List.map (conv_expr deps) el))
      member_ret
  | F.Box e0 -> mk_exp_box ~typ:e0.F.typ (conv_expr deps e0)
  | F.Unbox e0 -> mk_exp_unbox (conv_expr deps e0)
  | F.Tag e0 -> mk_exp_vmem (conv_expr deps e0) member_tag
  | F.Proj (e0, n, i) -> mk_exp_proj (conv_expr deps e0) n i

and conv_op deps op =
  let aux typ mk id_cmp e1 e2 =
    let e1' = conv_expr deps e1 in
    let e2' = conv_expr deps e2 in
    match typ with
    | EmlType.Tconstr ("__ml_boxed", _) ->
      mk_exp_op (mk (mk_exp_unbox e1') (mk_exp_unbox e2'))
    | _ when EmlType.is_basetype typ -> mk_exp_op (mk e1' e2')
    | _ -> mk_exp_vmem (mk_exp_app (mk_exp_var id_cmp) [e1'; e2']) member_val
  in
  match op with
  | EmlOp.Eq (e1, e2) ->
    aux e1.F.typ (fun e1 e2 -> EmlOp.Eq (e1, e2)) "__ml_eq" e1 e2
  | EmlOp.Ne (e1, e2) ->
    aux e1.F.typ (fun e1 e2 -> EmlOp.Ne (e1, e2)) "__ml_ne" e1 e2
  | EmlOp.Ge (e1, e2) ->
    aux e1.F.typ (fun e1 e2 -> EmlOp.Ge (e1, e2)) "__ml_ge" e1 e2
  | EmlOp.Le (e1, e2) ->
    aux e1.F.typ (fun e1 e2 -> EmlOp.Le (e1, e2)) "__ml_le" e1 e2
  | EmlOp.Gt (e1, e2) ->
    aux e1.F.typ (fun e1 e2 -> EmlOp.Gt (e1, e2)) "__ml_gt" e1 e2
  | EmlOp.Lt (e1, e2) ->
    aux e1.F.typ (fun e1 e2 -> EmlOp.Lt (e1, e2)) "__ml_lt" e1 e2
  | op -> mk_exp_op (EmlOp.map (conv_expr deps) op)

and conv_let_expr deps (lets, e) =
  let (deps', lets') = List.fold_map conv_let_expr_desc deps lets in
  (lets', conv_expr deps' e)

and conv_let_expr_desc deps = function
  | F.Let_fun (_, id, _, opt_deps, e) ->
    let deps' = List.filter_map identity opt_deps @ deps in
    let (lets', e') = conv_let_expr deps' e in
    let d_fun = Template_class (member_fun, opt_deps, lets', mk_decl_ret e') in
    let deps' = if e'.dep then id :: deps' else deps' in
    (deps', Class (id, [], [d_fun]))
  | F.Let_val (id, _, e) ->
    match e.F.data with
    | F.Box e0 when EmlType.is_basetype (EmlType.observe e0.F.typ) ->
      (* let id = __ml_box e0 *)
      let e0' = conv_expr deps e0 in
      let decl = Class (id, [], [mk_decl_tag (-1);
                                 Static (member_val, e0.F.typ, e0')]) in
      let deps' = if e0'.dep then id :: deps else deps in
      (deps', decl)
    | _ when EmlType.is_basetype (EmlType.observe e.F.typ) ->
      (* let id = (e : basetype) *)
      let e' = conv_expr deps e in
      let decl = Static (id, e.F.typ, e') in
      let deps' = if e'.dep then id :: deps else deps in
      (deps', decl)
    | _ ->
      let e' = conv_expr deps e in
      let decl = mk_decl_typedef id e' in
      let deps' = if e'.dep then id :: deps else deps in
      (deps', decl)

let conv_constr (tag, id, t_deps) =
  let id = get_constr_name id in
  let d_tag = mk_decl_tag tag in
  let s_deps = List.mapi (fun i _ -> "x" ^ string_of_int i) t_deps in
  match s_deps with
  | [] -> Class (id, [], [d_tag])
  | [x1] ->
    Template_class (id, [Some x1], [],
                    [d_tag;
                     mk_decl_typedef member_fst (mk_exp_var ~deps:s_deps x1)])
  | x1 :: xs ->
    let e = mk_exp_tuple (List.map (fun s -> mk_exp_var ~deps:s_deps s) xs) in
    let pub = [d_tag;
               mk_decl_typedef member_fst (mk_exp_var ~deps:s_deps x1);
               mk_decl_typedef member_snd e] in
    Template_class (id, List.map (fun s -> Some s) s_deps, [], pub)

let convert ~header =
  let aux rev_tops = function
    | F.Top_let led -> snd (conv_let_expr_desc [] led) :: rev_tops
    | F.Top_type (EmlType.Variant (_, _, constrs)) ->
      let constrs' = List.rev_map conv_constr constrs in
      constrs' @ rev_tops
    | F.Top_code s -> Code s :: rev_tops
  in
  List.fold_left aux [Code header] >> List.rev

(** {2 Pretty printing} *)

let pp_list_line pp =
  pp_list ~pp_delim:(fun ppf -> pp_force_newline ppf ()) pp

let pp_template_arg ppf = function
  | None -> pp_print_string ppf "class"
  | Some s -> fprintf ppf "class %s" s

let rec pp_expr ppf e = match e.data with
  | Error | Const EmlSyntax.Unit -> pp_print_string ppf "void"
  | Const (EmlSyntax.Bool b) -> pp_print_bool ppf b
  | Const (EmlSyntax.Char c) -> fprintf ppf "%d" (int_of_char c)
  | Const (EmlSyntax.Int n) -> pp_print_int ppf n
  | Const (EmlSyntax.Float x) -> pp_print_float ppf x
  | Var id -> pp_print_string ppf id
  | Tmember (b, e0, field) ->
    fprintf ppf "%a::@;<0 2>%s%s"
      pp_expr e0 (if b then "template " else "") field
  | Vmember (e0, field) -> fprintf ppf "%a::@;<0 2>%s" pp_expr e0 field
  | Op (EmlOp.Not e1) -> fprintf ppf "!@[%a@]" pp_expr e1
  | Op (EmlOp.And(e1,e2)) -> fprintf ppf "(@[%a@ && %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Or (e1,e2)) -> fprintf ppf "(@[%a@ || %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Pos e1) | Op (EmlOp.FPos e1) -> fprintf ppf "+@[%a@]" pp_expr e1
  | Op (EmlOp.Neg e1) | Op (EmlOp.FNeg e1) -> fprintf ppf "-@[%a@]" pp_expr e1
  | Op (EmlOp.Add (e1, e2)) | Op (EmlOp.FAdd (e1, e2)) ->
    fprintf ppf "(@[%a@ + %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Sub (e1, e2)) | Op (EmlOp.FSub (e1, e2)) ->
    fprintf ppf "(@[%a@ - %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Mul (e1, e2)) | Op (EmlOp.FMul (e1, e2)) ->
    fprintf ppf "(@[%a@ * %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Div (e1, e2)) | Op (EmlOp.FDiv (e1, e2)) ->
    fprintf ppf "(@[%a@ / %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Mod(e1,e2)) -> fprintf ppf "(@[%a@ %% %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Eq(e1,e2)) -> fprintf ppf "(@[%a@ == %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Ne(e1,e2)) -> fprintf ppf "(@[%a@ != %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Lt(e1,e2)) -> fprintf ppf "(@[%a@ < %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Gt(e1,e2)) -> fprintf ppf "(@[%a@ > %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Le(e1,e2)) -> fprintf ppf "(@[%a@ <= %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Ge(e1,e2)) -> fprintf ppf "(@[%a@ >= %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Lnot e1) -> fprintf ppf "(@[~ %a@])" pp_expr e1
  | Op (EmlOp.Land(e1,e2)) -> fprintf ppf "(@[%a@ & %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Lor(e1,e2)) -> fprintf ppf "(@[%a@ | %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Lxor(e1,e2)) -> fprintf ppf "(@[%a@ ^ %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Lsl(e1,e2)) ->
    fprintf ppf "(@[(unsigned int)@ %a@ << %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Lsr(e1,e2)) ->
    fprintf ppf "(@[(unsigned int)@ %a@ >> %a@])" pp_expr e1 pp_expr e2
  | Op (EmlOp.Asr(e1,e2)) ->
    fprintf ppf "(@[(signed int)@ %a@ >> %a@])" pp_expr e1 pp_expr e2
  | App (e0, el) ->
    let sp = match List.last el with
      | (_, { data = App _; _ }) -> " "
      | _ -> "" in
    fprintf ppf "%a@;<0 2><@[%a@]%s>"
      pp_expr e0 (pp_list_comma pp_type_expr) el sp

and pp_type_expr ppf = function
  | (false, e) -> pp_expr ppf e
  | (true, e) -> fprintf ppf "typename@;<1 2>@[%a@]" pp_expr e

let rec pp_decl ppf = function
  | Code s -> pp_print_string ppf s
  | Static (id, t, e) ->
    fprintf ppf "static const %a %s = %a;" EmlType.pp t id pp_expr e
  | EmlTypedef (id, e) ->
    fprintf ppf "typedef %a %s;" pp_type_expr e id
  | Class (id, priv, pub) -> pp_class ppf id priv pub
  | Template_class (id, args, priv, pub) ->
    fprintf ppf "template <@[%a@]>@\n" (pp_list_comma pp_template_arg) args;
    pp_class ppf id priv pub

and pp_class ppf id priv pub =
  match priv with
  | [] ->
    fprintf ppf "struct %s {@\n  @[%a@]@\n};" id (pp_list_line pp_decl) pub
  | _ ->
    fprintf ppf "class %s {@\n\
                 private:@\n  @[%a@]@\n\
                 public:@\n  @[%a@]@\n\
                 };"
      id (pp_list_line pp_decl) priv (pp_list_line pp_decl) pub
