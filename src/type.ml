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

open Utils
open Format

type type_var = int

type t =
  | Unit
  | Bool
  | Char
  | Int
  | Float
  | Arrow of t list * t
  | Tuple of t list
  | Tconstr of string * t list
  | Var of string option * type_var
  | Ref of t ref (* for destructive unification *)

(** {2 Types} *)

let genvar =
  let c = ref 0 in
  fun ?name () -> incr c ; Ref (ref (Var (name, !c)))

let rec observe = function
  | Ref r -> observe !r
  | t -> t

let name_of_int n =
  let chrs = "abcdefghijklmnopqrstuvwxyz" in
  let m = String.length chrs in
  let b = Buffer.create 2 in
  let rec aux n =
    if n > 0 then begin Buffer.add_char b chrs.[n mod m] ; aux (n / m) end
    else Buffer.contents b
  in
  if n = 0 then "a" else aux n

let is_basetype t = match observe t with
  | Bool | Char | Int | Float -> true
  | _ -> false

let unarrow t = match observe t with
  | Arrow (args, ret) -> Some (args, ret)
  | _ -> None

let get_var_name =
  let tbl = ref [] in
  fun (x : type_var) ->
    try List.assoc x !tbl
    with Not_found ->
      let name = "'" ^ name_of_int (List.length !tbl) in
      tbl := (x, name) :: !tbl;
      name

let rec pp ppf t =
  let rec aux b ppf t = match observe t with
    | Ref _ -> assert false
    | Var (_, i) -> pp_var ppf i
    | Unit -> pp_print_string ppf "unit"
    | Bool -> pp_print_string ppf "bool"
    | Char -> pp_print_string ppf "char"
    | Int -> pp_print_string ppf "int"
    | Float -> pp_print_string ppf "float"
    | Tuple tl -> fprintf ppf "(@[%a@])" (pp_tuple (aux true)) tl
    | Tconstr (s, []) -> pp_print_string ppf s
    | Tconstr (s, [t]) -> fprintf ppf "@[%a@] %s" (aux true) t s
    | Tconstr (s, tl) -> fprintf ppf "(@[%a@]) %s"
                           (pp_list_comma (aux false)) tl s
    | Arrow (args, ret) ->
      if b then pp_print_char ppf '(';
      List.iter (fprintf ppf "%a -> " (aux true)) args;
      aux false ppf ret;
      if b then pp_print_char ppf ')'
  in
  aux false ppf t

and pp_tuple pp =
  let pp_delim ppf =
    pp_print_space ppf ();
    pp_print_string ppf "* "
  in
  pp_list ~pp_delim pp

and pp_var ppf i = pp_print_string ppf (get_var_name i)

let rec occurs_check x t = match observe t with
  | Ref _ -> assert false
  | Var (_, y) -> x != y
  | Unit | Bool | Char | Int | Float -> true
  | Arrow (args, ret) -> List.for_all (occurs_check x) (ret :: args)
  | Tuple tl | Tconstr (_, tl) -> List.for_all (occurs_check x) tl

(** [bind x t] binds type variable [x] to type [t]. *)
let rec bind = function
  | Ref { contents = (Ref _ as t) } -> bind t
  | Ref ({ contents = Var _ } as r) -> fun t -> r := t
  | _ -> failwith "Type.bind"

let unify ~loc t0 u0 =
  let rec aux t u = match observe t, observe u with
    | Unit, Unit -> ()
    | Bool, Bool -> ()
    | Char, Char -> ()
    | Int, Int -> ()
    | Float, Float -> ()
    | Var (_, x), Var (_, y) when x == y -> ()
    | Var (_, x), _ when occurs_check x u -> bind t u
    | _, Var (_, y) when occurs_check y t -> bind u t
    | Tuple tl, Tuple ul -> List.iter2 aux tl ul
    | Tconstr (ts, tl), Tconstr (us, ul) when ts = us ->
      (try List.iter2 aux tl ul
       with Invalid_argument _ ->
         errorf ~loc "The type constructor %s expects %d argument(s), \
                      but is here applied to %d argument(s)"
           ts (List.length tl) (List.length ul) ())
    | Arrow (t1 :: tl, tr), Arrow (u1 :: ul, ur) ->
      aux t1 u1 ; aux (Arrow (tl, tr)) (Arrow (ul, ur))
    | Arrow ([], tr), Arrow ([], ur) -> aux tr ur
    | Arrow ([], tr), Arrow _ -> aux tr u
    | Arrow _, Arrow ([], ur) -> aux t ur
    | _ -> errorf ~loc "This expression has type %a\n\
                        but an expression was expected of type %a\n\
                        Type %a is not compatible with %a"
             pp t0 pp u0 pp t pp u ()
  in
  aux t0 u0

(** {2 Type scheme} *)

module VarsSet = Set.Make(struct
    type t = type_var
    let compare = Pervasives.compare
  end)

type scheme = VarsSet.t * t

type context = (string * scheme) list

let scheme t = (VarsSet.empty, t)

let free_vars_in_type =
  let rec aux acc t = match observe t with
    | Ref _ -> assert false
    | Unit | Bool | Char | Int | Float -> acc
    | Var (_, i) -> VarsSet.add i acc
    | Tuple tl -> List.fold_left aux acc tl
    | Tconstr (_, tl) -> List.fold_left aux acc tl
    | Arrow (args, ret) -> List.fold_left aux (aux acc ret) args
  in
  aux VarsSet.empty

let free_vars_in_scheme (bound_vars, t) =
  let free_vars = free_vars_in_type t in
  VarsSet.diff free_vars bound_vars

let free_vars_in_context ctx =
  List.fold_left (fun acc (_, ts) -> VarsSet.union acc (free_vars_in_scheme ts))
    VarsSet.empty ctx

(** [subst_vars vars t0] substitutes type variables [vars] in [t0] for fresh
    variables. *)
let subst_vars vars t0 =
  let tbl = List.map (fun i -> (i, genvar ())) (VarsSet.elements vars) in
  let rec aux t = match observe t with
    | Ref _ -> assert false
    | Unit | Bool | Char | Int | Float -> t
    | Var (_, i) -> if VarsSet.mem i vars then List.assoc i tbl else t
    | Tuple tl -> Tuple (List.map aux tl)
    | Tconstr (s, tl) -> Tconstr (s, List.map aux tl)
    | Arrow (args, ret) -> Arrow (List.map aux args, aux ret)
  in
  let var_num (_, t) = match observe t with
    | Var (_, i) -> i
    | _ -> assert false
  in
  (VarsSet.of_list (List.map var_num tbl), aux t0)

let generalize ctx t =
  let bound_vars = free_vars_in_context ctx in
  let free_vars = free_vars_in_type t in
  (VarsSet.diff free_vars bound_vars, t)

let instantiate (vars, t) = subst_vars vars t |> snd

let pp_scheme ppf (vars, t) =
  match VarsSet.elements vars with
  | [] -> pp ppf t
  | l -> fprintf ppf "@[forall @[%a@].@;<1 2>@[%a@]@]"
           (pp_list_comma pp_var) l pp t

(** {2 Typing contexts} *)

let lookup ~loc s ctx =
  try List.assoc s ctx
  with Not_found -> errorf ~loc "Unbound identifier `%s'" s ()
