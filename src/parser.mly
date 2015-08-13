%{
open Utils
open Format
open Syntax
open Location

let check_dup ~loc l =
  match List.duplicated l with
  | [] -> ()
  | dups -> errorf ~loc "Duplicated identifier(s): %a"
              (pp_list_comma pp_print_string) dups ()

let check_dup_args ~loc args =
  check_dup ~loc (List.filter_map (fun x -> x) args)

let mk ?(loc = from_symbol ()) data = { loc; data; }

let mk_exp_unary_plus e =
  match e.data with
  | Const (Syntax.Int n) -> mk (Const (Syntax.Int n))
  | Const (Syntax.Float n) -> mk (Const (Syntax.Float n))
  | _ -> mk (Op (Op.Pos e))

let mk_exp_unary_minus e =
  match e.data with
  | Const (Syntax.Int n) -> mk (Const (Syntax.Int (~- n)))
  | Const (Syntax.Float n) -> mk (Const (Syntax.Float (~-. n)))
  | _ -> mk (Op (Op.Neg e))

let mk_exp_cons ?loc e1 e2 = Constr ("::", [e1; e2]) |> mk ?loc

let mk_exp_list ?loc elms =
  let nil = Constr ("[]", []) |> mk ?loc in
  List.fold_left (fun e x -> mk_exp_cons ?loc x e) nil elms

let mk_exp_string ?loc s =
  String.to_list s
  |> List.map (fun c -> Const (Char c) |> mk ?loc)
  |> List.rev
  |> mk_exp_list ?loc

let mk_pat_list ?loc elms =
  let nil = Pconstr ("[]", []) |> mk ?loc in
  List.fold_left (fun e x -> Pconstr ("::", [x; e]) |> mk ?loc) nil elms

let mk_pat_string ?loc s =
  String.to_list s
  |> List.map (fun c -> Pconst (Pchar c) |> mk ?loc)
  |> List.rev
  |> mk_pat_list ?loc

let scope_tyvars tbl =
  let rec aux t = match t with
    | Type.Unit | Type.Bool | Type.Char | Type.Int | Type.Float -> t
    | Type.Var (None, _) -> t
    | Type.Var (Some s, _) ->
      if Hashtbl.mem tbl s then Type.Var (Some s, Hashtbl.find tbl s)
      else failwith ("Unbound type parameter " ^ s)
    | Type.Arrow (args, ret) -> Type.Arrow (List.map aux args, aux ret)
    | Type.Tuple tl -> Type.Tuple (List.map aux tl)
    | Type.Tconstr (s, tl) -> Type.Tconstr (s, List.map aux tl)
  in
  List.map aux

let mk_type ?(loc = Location.from_symbol ()) name rev_args rev_constrs =
  check_dup_args ~loc rev_args;
  check_dup ~loc (List.map fst rev_constrs);
  let tbl = Hashtbl.create 4 in
  let args = rev_args
             |> List.rev
             |> List.map (fun name -> Type.genvar ?name ()) in
  List.iter (function
      | Type.Var (Some s, r) -> Hashtbl.add tbl s r
      | _ -> ()) args;
  let constrs = rev_constrs
                |> List.rev
                |> List.map (fun (name, t) ->
                    (name, scope_tyvars tbl t)) in
  Top_variant_type (name, args, constrs) |> mk ~loc

(** [find_type name tops] finds the declaration of type [name]. *)
let find_type name =
  let aux top = match top.data with
    | Top_variant_type (s, args, constrs) when s = name -> Some (args, constrs)
    | _ -> None
  in
  List.find_map aux

(** [check_type ~loc tops t] checks the number of type parameters of type
    constructors in type [t]. *)
let check_type ~loc tops =
  let rec aux t = match Type.observe t with
    | Type.Unit | Type.Bool | Type.Char | Type.Int | Type.Float
    | Type.Var _ -> ()
    | Type.Arrow (args, ret) -> List.iter aux (ret :: args)
    | Type.Tuple tl -> List.iter aux tl
    | Type.Tconstr (name, args) ->
      match find_type name tops with
      | None -> errorf ~loc "The type constructor %s is not defined" name ()
      | Some (args', _) ->
        let m, n = List.length args, List.length args' in
        if m <> n
        then errorf ~loc "The type constructor %s expects %d argument(s), \
                          but is here applied to %d argument(s)" name n m ()
  in
  aux

let check_constrs ~loc tops =
  List.iter (snd >> List.iter (check_type ~loc tops))

let check_top_shadowing tops =
  let aux (types, vars) { loc; data; } = match data with
    | Top_variant_type (s, _, constrs) ->
      (* Check top-level shadowing of types and constructors. *)
      if List.mem s types then errorf ~loc "Type %s is already defined" s ();
      let names = List.map fst constrs in
      List.iter (fun s ->
          if List.mem s vars
          then errorf ~loc "Constructor %s is already defined" s ()) names;
      (s :: types, names @ vars)
    | Top_let (_, s, _) ->
      (* Check top-level shadowing of variables. *)
      if List.mem s vars
      then errorf ~loc "Top-level identifier %s is already defined" s ();
      (types, s :: vars)
    | Top_code _ -> (types, vars)
  in
  ignore (List.fold_left aux ([], []) tops)
%}

%token <string> CPP_CODE
%token <bool> VBOOL
%token <char> VCHAR
%token <int> VINT
%token <float> VFLOAT
%token <string> VSTRING
%token ERROR
%token ANDAND BARBAR NOT
%token EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%token PLUS MINUS STAR SLASH MOD PLUSDOT MINUSDOT STARDOT SLASHDOT
%token UNIT BOOL CHAR INT FLOAT ARROW
%token BEGIN END
%token COLON
%token COLONCOLON
%token COMMA
%token ELSE
%token EOF
%token FUN
%token IF
%token IN
%token LBRACKET
%token LET
%token <string> LIDENT
%token LPAREN
%token MATCH
%token SEMICOLON
%token THEN
%token TYPE
%token OF
%token BAR
%token QUOTE
%token RBRACKET
%token REC
%token RPAREN
%token UNDERSCORE
%token <string> UIDENT
%token WITH

%nonassoc prec_fun prec_let prec_match
%nonassoc prec_if
%right ARROW
%nonassoc COMMA
%left BAR
%right BARBAR
%right ANDAND
%left EQUAL LESS GREATER LESS_GREATER LESS_EQUAL GREATER_EQUAL
%right COLONCOLON SEMICOLON
%left PLUS MINUS PLUSDOT MINUSDOT
%left STAR STARDOT SLASH SLASHDOT MOD
%nonassoc NOT prec_unary_plus prec_unary_minus
%left prec_app

%start main
%type <Syntax.top list> main
%%

main:
  toplevel EOF { let tops = List.rev $1 in check_top_shadowing tops ; tops }
| error        { errorf ~loc:(Location.from_symbol ()) "Syntax error" () }

/*********************************************************************
 * Toplevel
 *********************************************************************/

toplevel:
    { [] }
| toplevel toplevel_phrase
    { let tops = $2 :: $1 in
      (match $2.data with
       | Top_variant_type (_, _, cs) -> check_constrs ~loc:$2.loc tops cs
       | _ -> ());
      tops }

toplevel_phrase:
  CPP_CODE
    { mk (Top_code $1) }
| TYPE LIDENT EQUAL type_decl
    { mk_type $2 [] $4 }
| TYPE type_var LIDENT EQUAL type_decl
    { mk_type $3 [$2] $5 }
| TYPE LPAREN formal_type_args RPAREN LIDENT EQUAL type_decl
    { mk_type $5 $3 $7 }
| LET LIDENT EQUAL expr
  %prec prec_let
    { mk (Top_let (false, $2, $4)) }
| LET LIDENT formal_args EQUAL expr
  %prec prec_let
    { check_dup_args ~loc:(Location.from_rhs 3) $3;
      mk (Top_let (false, $2, mk (Abs (List.rev $3, $5)))) }
| LET REC LIDENT formal_args EQUAL expr
  %prec prec_let
    { check_dup_args ~loc:(Location.from_rhs 4) $4;
      mk (Top_let (true, $3, mk (Abs (List.rev $4, $6)))) }

formal_type_args:
  type_var COMMA type_var         { [$3; $1] }
| formal_type_args COMMA type_var { $3 :: $1 }

type_decl:
  type_decl_constr               { [$1] }
| BAR type_decl_constr           { [$2] }
| type_decl BAR type_decl_constr { $3 :: $1 }

type_decl_constr:
  vconstr_ident                { ($1, []) }
| vconstr_ident OF simple_type { ($1, [$3]) }
| vconstr_ident OF tuple_type  { ($1, List.rev $3) }

vconstr_ident:
  UIDENT            { $1 }
| LBRACKET RBRACKET { "[]" }
| COLONCOLON        { "::" }

/*********************************************************************
 * Types
 *********************************************************************/

type_expr:
  simple_type { $1 }
| tuple_type  { Type.Tuple (List.rev $1) }
| arrow_type  { let (args, ret) = $1 in Type.Arrow (args, ret) }

arrow_type:
  simple_type ARROW simple_type { ([$1], $3) }
| simple_type ARROW arrow_type  { let (args, ret) = $3 in ($1 :: args, ret) }

tuple_type:
  simple_type STAR simple_type { [$3; $1] }
| tuple_type STAR simple_type  { $3 :: $1 }

simple_type:
  type_var                              { Type.genvar ?name:$1 () }
| UNIT                                  { Type.Unit }
| BOOL                                  { Type.Bool }
| CHAR                                  { Type.Char }
| INT                                   { Type.Int }
| FLOAT                                 { Type.Float }
| LIDENT                                { Type.Tconstr ($1, []) }
| simple_type LIDENT                    { Type.Tconstr ($2, [$1]) }
| LPAREN actual_type_args RPAREN LIDENT { Type.Tconstr ($4, List.rev $2) }
| LPAREN type_expr RPAREN               { $2 }

actual_type_args:
  type_expr COMMA type_expr        { [$3; $1] }
| actual_type_args COMMA type_expr { $3 :: $1 }

type_var:
  UNDERSCORE   { None }
| QUOTE LIDENT { Some ("'" ^ $2) }
| QUOTE UIDENT { Some ("'" ^ $2) }

/*********************************************************************
 * Expressions
 *********************************************************************/

expr:
  app_expr
    { $1 }
| FUN formal_args ARROW expr
  %prec prec_fun
    { check_dup_args ~loc:(Location.from_rhs 2) $2;
      mk (Abs (List.rev $2, $4)) }
| IF expr THEN expr ELSE expr
  %prec prec_if
    { mk (If ($2, $4, $6)) }
| LET LIDENT EQUAL expr IN expr
  %prec prec_let
    { mk (Let (false, $2, $4, $6)) }
| LET LIDENT formal_args EQUAL expr IN expr
  %prec prec_let
    { check_dup_args ~loc:(Location.from_rhs 3) $3;
      mk (Let (false, $2, mk (Abs (List.rev $3, $5)), $7)) }
| LET REC LIDENT formal_args EQUAL expr IN expr
  %prec prec_let
    { check_dup_args ~loc:(Location.from_rhs 4) $4;
      mk (Let (true, $3, mk (Abs (List.rev $4, $6)), $8)) }
| MATCH expr WITH match_cases
  %prec prec_match
    { mk (Match ($2, List.rev $4)) }
| expr COLONCOLON expr                 { mk_exp_cons $1 $3 }
| expr ANDAND expr                     { mk (Op (Op.And ($1, $3))) }
| expr BARBAR expr                     { mk (Op (Op.Or ($1, $3))) }
| expr EQUAL expr                      { mk (Op (Op.Eq ($1, $3))) }
| expr LESS expr                       { mk (Op (Op.Lt ($1, $3))) }
| expr GREATER expr                    { mk (Op (Op.Gt ($1, $3))) }
| expr LESS_EQUAL expr                 { mk (Op (Op.Le ($1, $3))) }
| expr GREATER_EQUAL expr              { mk (Op (Op.Ge ($1, $3))) }
| expr LESS_GREATER expr               { mk (Op (Op.Ne ($1, $3))) }
| expr PLUS expr                       { mk (Op (Op.Add ($1, $3))) }
| expr MINUS expr                      { mk (Op (Op.Sub ($1, $3))) }
| expr STAR expr                       { mk (Op (Op.Mul ($1, $3))) }
| expr SLASH expr                      { mk (Op (Op.Div ($1, $3))) }
| expr MOD expr                        { mk (Op (Op.Mod ($1, $3))) }
| expr PLUSDOT expr                    { mk (Op (Op.FAdd ($1, $3))) }
| expr MINUSDOT expr                   { mk (Op (Op.FSub ($1, $3))) }
| expr STARDOT expr                    { mk (Op (Op.FMul ($1, $3))) }
| expr SLASHDOT expr                   { mk (Op (Op.FDiv ($1, $3))) }
| NOT expr                             { mk (Op (Op.Not $2)) }
| PLUSDOT expr  %prec prec_unary_plus  { mk (Op (Op.FPos $2)) }
| MINUSDOT expr %prec prec_unary_minus { mk (Op (Op.FNeg $2)) }
| PLUS expr     %prec prec_unary_plus  { mk_exp_unary_plus $2 }
| MINUS expr    %prec prec_unary_minus { mk_exp_unary_minus $2 }

app_expr:
  UIDENT             { mk (Constr ($1, [])) }
| UIDENT simple_expr { match $2.data with
                       | Tuple l -> mk (Constr ($1, l))
                       | _ -> mk (Constr ($1, [$2])) }
| fun_app_expr       { match $1 with
                       | (e, []) -> e
                       | (e, l) -> mk (App (e, List.rev l)) }

fun_app_expr:
  simple_expr
    { ($1, []) }
| fun_app_expr simple_expr
  %prec prec_app
    { match $1 with (f, args) -> (f, $2 :: args) }

simple_expr:
  LPAREN RPAREN                      { mk (Const Unit) }
| VBOOL                              { mk (Const (Bool $1)) }
| VCHAR                              { mk (Const (Char $1)) }
| VINT                               { mk (Const (Int $1)) }
| VFLOAT                             { mk (Const (Float $1)) }
| VSTRING                            { mk_exp_string $1 }
| LIDENT                             { mk (Var $1) }
| LPAREN expr COLON type_expr RPAREN { mk (Constraint ($2, $4)) }
| LPAREN exprs_comma RPAREN          { mk (Tuple (List.rev $2)) }
| LBRACKET RBRACKET                  { mk (Constr ("[]", [])) }
| LBRACKET exprs_semi RBRACKET       { mk_exp_list $2 }
| ERROR                              { mk Error }
| LPAREN expr RPAREN                 { $2 }
| BEGIN expr END                     { $2 }

exprs_comma:
  expr COMMA expr        { [$3; $1] }
| exprs_comma COMMA expr { $3 :: $1 }

exprs_semi:
  expr                      { [$1] }
| exprs_semi SEMICOLON expr { $3 :: $1 }

formal_args:
  formal_arg             { [$1] }
| formal_args formal_arg { $2 :: $1 }

formal_arg:
  UNDERSCORE { None }
| LIDENT     { Some $1 }

match_cases:
  pattern ARROW expr                 { [($1, $3)] }
| BAR pattern ARROW expr             { [($2, $4)] }
| match_cases BAR pattern ARROW expr { ($3, $5) :: $1 }

/*********************************************************************
 * Patterns
 *********************************************************************/

pattern:
  simple_pattern
    { $1 }
| UIDENT simple_pattern
    { let args = match $2.data with Ptuple l -> l | _ -> [$2] in
      mk (Pconstr ($1, args)) }

simple_pattern:
  UNDERSCORE                               { mk (Pvar None) }
| LIDENT                                   { mk (Pvar (Some $1)) }
| LPAREN RPAREN                            { mk (Pconst Punit) }
| VBOOL                                    { mk (Pconst (Pbool $1)) }
| VCHAR                                    { mk (Pconst (Pchar $1)) }
| VINT                                     { mk (Pconst (Pint $1)) }
| VSTRING                                  { mk_pat_string $1 }
| UIDENT                                   { mk (Pconstr ($1, [])) }
| LPAREN tuple_pattern RPAREN              { mk (Ptuple (List.rev $2)) }
| LBRACKET RBRACKET                        { mk (Pconstr ("[]", [])) }
| simple_pattern COLONCOLON simple_pattern { mk (Pconstr ("::", [$1; $3])) }
| LBRACKET list_pattern RBRACKET           { mk_pat_list $2 }
| LPAREN pattern RPAREN                    { $2 }

tuple_pattern:
  pattern COMMA pattern       { [$3; $1] }
| tuple_pattern COMMA pattern { $3 :: $1 }

list_pattern:
  pattern                        { [$1] }
| list_pattern SEMICOLON pattern { $3 :: $1 }
