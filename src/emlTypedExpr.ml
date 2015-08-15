open Format
open EmlUtils

module S = EmlSyntax

(** {2 EmlTyped expressions} *)

type 'a typed =
  {
    loc : EmlLocation.t;
    typ : EmlType.t;
    data : 'a;
  }
    [@@deriving show]

type 'a base_expr = 'a expr_desc typed
and 'a expr_desc =
  | Error
  | Const of S.const
  | Var of string
  | Constr of string * 'a base_expr list
  | Tuple of 'a base_expr list
  | If of 'a base_expr * 'a base_expr * 'a base_expr
  | Op of 'a base_expr EmlOp.t
  | App of 'a base_expr * 'a base_expr list
  | Abs of string option list * 'a base_expr
  | Let of bool * string * EmlType.scheme * 'a base_expr * 'a base_expr
  | Ext of 'a (* extended *)
        [@@deriving show]

let mk_exp_error ~loc () = { loc; typ = EmlType.genvar (); data = Error; }
let mk_exp_unit ~loc () = { loc; typ = EmlType.Unit; data = Const S.Unit; }
let mk_exp_bool ~loc b = { loc; typ = EmlType.Bool; data = Const (S.Bool b); }
let mk_exp_char ~loc c = { loc; typ = EmlType.Char; data = Const (S.Char c); }
let mk_exp_int ~loc n = { loc; typ = EmlType.Int; data = Const (S.Int n); }
let mk_exp_float ~loc x = { loc; typ = EmlType.Float; data = Const (S.Float x); }
let mk_exp_var ~loc id t = { loc; typ = t; data = Var id; }
let mk_exp_constr ~loc id t el = { loc; typ = t; data = Constr (id, el); }

let mk_exp_var_lookup ~loc ctx id =
  let tysc = EmlType.lookup ~loc id ctx in
  mk_exp_var ~loc id (EmlType.instantiate tysc)

let mk_exp_constr_lookup ~loc ctx id el =
  let tysc = EmlType.lookup ~loc id ctx in
  let t_args = List.map (fun ei -> ei.typ) el in
  let t_ret = EmlType.genvar () in
  match EmlType.unarrow (EmlType.instantiate tysc) with
  | None -> errorf ~loc "Constructor %s has illegal type %a"
              id EmlType.pp_scheme tysc ()
  | Some (u_args, u_ret) ->
    let m, n = List.length t_args, List.length u_args in
    if m <> n then errorf ~loc "Constructor %s expects %d argument(s) \
                                but %d argument(s) are applied" id n m ();
    List.iter2 (EmlType.unify ~loc) u_args t_args;
    EmlType.unify ~loc u_ret t_ret;
    mk_exp_constr ~loc id t_ret el

let mk_exp_tuple ~loc el =
  let tl = List.map (fun ei -> ei.typ) el in
  { loc; typ = EmlType.Tuple tl; data = Tuple el; }

let mk_exp_op ~loc op =
  let typ = match op with
    (* comparison operators (polymorphic) *)
    | EmlOp.Eq (e1, e2) | EmlOp.Ne (e1, e2) | EmlOp.Gt (e1, e2)
    | EmlOp.Lt (e1, e2) | EmlOp.Ge (e1, e2) | EmlOp.Le (e1, e2) ->
      EmlType.unify ~loc e1.typ e2.typ;
      EmlType.Bool
    (* boolean operators *)
    | EmlOp.Not e1 ->
      EmlType.unify ~loc:e1.loc e1.typ EmlType.Bool;
      EmlType.Bool
    | EmlOp.And (e1, e2) | EmlOp.Or (e1, e2) ->
      EmlType.unify ~loc:e1.loc e1.typ EmlType.Bool;
      EmlType.unify ~loc:e2.loc e2.typ EmlType.Bool;
      EmlType.Bool
    (* integer operators *)
    | EmlOp.Pos e1 | EmlOp.Neg e1 ->
      EmlType.unify ~loc:e1.loc e1.typ EmlType.Int;
      EmlType.Int
    | EmlOp.Add (e1, e2) | EmlOp.Sub (e1, e2) | EmlOp.Mul (e1, e2)
    | EmlOp.Div (e1, e2) | EmlOp.Mod (e1, e2) ->
      EmlType.unify ~loc:e1.loc e1.typ EmlType.Int;
      EmlType.unify ~loc:e2.loc e2.typ EmlType.Int;
      EmlType.Int
    (* floating-point-value operators *)
    | EmlOp.FPos e1 | EmlOp.FNeg e1 ->
      EmlType.unify ~loc:e1.loc e1.typ EmlType.Float;
      EmlType.Int
    | EmlOp.FAdd (e1, e2) | EmlOp.FSub (e1, e2) | EmlOp.FMul (e1, e2)
    | EmlOp.FDiv (e1, e2) ->
      EmlType.unify ~loc:e1.loc e1.typ EmlType.Float;
      EmlType.unify ~loc:e2.loc e2.typ EmlType.Float;
      EmlType.Float
  in
  { loc; typ; data = Op op; }

let mk_exp_if ~loc e1 e2 e3 =
  EmlType.unify ~loc e1.typ EmlType.Bool;
  EmlType.unify ~loc e2.typ e3.typ;
  { loc; typ = e2.typ; data = If (e1, e2, e3); }

let mk_exp_app ~loc e_fun e_args =
  let t_args = List.map (fun ei -> ei.typ) e_args in
  let t_ret = EmlType.genvar () in
  let t_fun = EmlType.Arrow (t_args, t_ret) in
  EmlType.unify ~loc e_fun.typ t_fun;
  { loc; typ = t_ret; data = App (e_fun, e_args); }

let mk_exp_abs ?arg_types ~loc ctx f args e_body =
  let t_args = match arg_types with
    | None -> List.map (fun _ -> EmlType.genvar ()) args
    | Some t_args -> t_args in
  let ctx' = List.fold_left2 (fun acc t -> function
      | Some x -> (x, EmlType.scheme t) :: acc
      | None -> acc) ctx t_args args in
  let e_body' = f ctx' e_body in
  let t_fun = EmlType.Arrow (t_args, e_body'.typ) in
  { loc; typ = t_fun; data = Abs (args, e_body'); }

let mk_exp_let_rhs ~loc ctx f rf id e1 =
  let e1' = if not rf then f ctx e1 else begin
      let tx = EmlType.genvar () in
      let e1' = f ((id, EmlType.scheme tx) :: ctx) e1 in
      EmlType.unify ~loc tx e1'.typ;
      e1'
    end in
  let ts = EmlType.generalize ctx e1'.typ in
  (ts, e1')

let mk_exp_let ~loc ctx f rf id e1 e2 =
  let (ts, e1') = mk_exp_let_rhs ~loc ctx f rf id e1 in
  let e2' = f ((id, ts) :: ctx) e2 in
  { loc; typ = e2'.typ; data = Let (rf, id, ts, e1', e2'); }

let mk_exp_simple_let ~loc rf id e1 e2 =
  { loc; typ = e2.typ; data = Let (rf, id, EmlType.scheme e1.typ, e1, e2) }

(** {2 Top-level declaration} *)

module L = EmlLocation

type constr_tag = int [@@deriving show]

type 'a base_top = 'a top_desc EmlLocation.loc
and 'a top_desc =
  | Top_variant_type
    of string (* type name *)
       * EmlType.t list (* type parameters of type constructor *)
       * (constr_tag * string * EmlType.t list) list (* constructors *)
  | Top_let of bool * string * EmlType.scheme * 'a base_expr
  | Top_code of string
                 [@@deriving show]

let map f =
  let aux = function
    | Top_variant_type (name, args, cs) -> Top_variant_type (name, args, cs)
    | Top_let (rf, id, ts, e) -> Top_let (rf, id, ts, f e)
    | Top_code s -> Top_code s
  in
  List.map (L.map aux)

let fold_map f_vtype f_let init =
  let aux acc { L.loc; L.data; } = match data with
    | Top_variant_type (name, args, cs) ->
      let (acc', data) = f_vtype loc acc name args cs in
      (acc', { L.loc; L.data; })
    | Top_let (rf, id, ts, e) ->
      let (acc', data) = f_let loc acc rf id ts e in
      (acc', { L.loc; L.data; })
    | Top_code s -> (acc, { L.loc; L.data = Top_code s; })
  in
  List.fold_map aux init

let typeof_constrs name args cs =
  let t_ret = EmlType.Tconstr (name, args) in
  let aux (_, _, t_args) = EmlType.generalize [] (EmlType.Arrow (t_args, t_ret)) in
  List.map aux cs
