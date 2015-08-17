%{
open EmlUtils
open Format
open EmlSyntax
open EmlLocation

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
  | Const (EmlSyntax.Int n) -> mk (Const (EmlSyntax.Int n))
  | Const (EmlSyntax.Float n) -> mk (Const (EmlSyntax.Float n))
  | _ -> mk (EmlOp (EmlOp.Pos e))

let mk_exp_unary_minus e =
  match e.data with
  | Const (EmlSyntax.Int n) -> mk (Const (EmlSyntax.Int (~- n)))
  | Const (EmlSyntax.Float n) -> mk (Const (EmlSyntax.Float (~-. n)))
  | _ -> mk (EmlOp (EmlOp.Neg e))

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

let resolve_scope_tyvars ~loc tbl =
  let rec aux t = match EmlType.observe t with
    | EmlType.Ref _ -> assert false
    | EmlType.Unit | EmlType.Bool | EmlType.Char | EmlType.Int | EmlType.Float -> t
    | EmlType.Var (None, _) -> t
    | EmlType.Var (Some s, _) ->
      if Hashtbl.mem tbl s then EmlType.Var (Some s, Hashtbl.find tbl s)
      else errorf ~loc "Unbound type parameter %s" s ()
    | EmlType.Arrow (args, ret) -> EmlType.Arrow (List.map aux args, aux ret)
    | EmlType.Tuple tl -> EmlType.Tuple (List.map aux tl)
    | EmlType.Tconstr (s, tl) -> EmlType.Tconstr (s, List.map aux tl)
  in
  List.map aux

let mk_type ?(loc = EmlLocation.from_symbol ()) name rev_args rev_constrs =
  check_dup_args ~loc rev_args;
  check_dup ~loc (List.map fst rev_constrs);
  let tbl = Hashtbl.create 4 in
  let args = rev_args
             |> List.rev
             |> List.map (fun name -> EmlType.genvar ?name ()) in
  List.iter (fun t -> match EmlType.observe t with
      | EmlType.Var (Some s, i) -> Hashtbl.add tbl s i
      | _ -> ()) args;
  let constrs = rev_constrs
                |> List.rev
                |> List.map (fun (name, t) ->
                    (name, resolve_scope_tyvars ~loc tbl t)) in
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
  let rec aux t = match EmlType.observe t with
    | EmlType.Ref _ -> assert false
    | EmlType.Unit | EmlType.Bool | EmlType.Char | EmlType.Int | EmlType.Float
    | EmlType.Var _ -> ()
    | EmlType.Arrow (args, ret) -> List.iter aux (ret :: args)
    | EmlType.Tuple tl -> List.iter aux tl
    | EmlType.Tconstr (name, args) ->
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
      if List.mem s types then errorf ~loc "EmlType %s is already defined" s ();
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
    | Top_code _ | Top_use _ -> (types, vars)
  in
  ignore (List.fold_left aux ([], []) tops)
%}

%token <string> CPP_CODE
%token <bool> LITERAL_BOOL
%token <char> LITERAL_CHAR
%token <int> LITERAL_INT
%token <float> LITERAL_FLOAT
%token <string> LITERAL_STRING
%token ERROR
%token ANDAND BARBAR NOT
%token LAND LOR LXOR LNOT LSL LSR ASR
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
%token HASH_USE

%nonassoc prec_fun prec_let prec_match
%nonassoc prec_if
%nonassoc prec_tuple
%left COMMA
%right ARROW
%left BAR
%right BARBAR
%right ANDAND
%left EQUAL LESS GREATER LESS_GREATER LESS_EQUAL GREATER_EQUAL
%right COLONCOLON SEMICOLON
%left PLUS MINUS PLUSDOT MINUSDOT
%left STAR STARDOT SLASH SLASHDOT MOD LAND LOR LXOR
%right LSL LSR ASR
%nonassoc NOT LNOT prec_unary_plus prec_unary_minus
%left prec_app

%start main
%type <EmlSyntax.top list> main
%%

main:
  toplevel EOF { let tops = List.rev $1 in check_top_shadowing tops ; tops }
| error        { errorf ~loc:(EmlLocation.from_symbol ()) "syntax error" () }

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
| HASH_USE LITERAL_STRING
    { mk (Top_use $2) }
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
    { check_dup_args ~loc:(EmlLocation.from_rhs 3) $3;
      mk (Top_let (false, $2, mk (Abs (List.rev $3, $5)))) }
| LET REC LIDENT formal_args EQUAL expr
  %prec prec_let
    { check_dup_args ~loc:(EmlLocation.from_rhs 4) $4;
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
 * EmlTypes
 *********************************************************************/

type_expr:
  simple_type { $1 }
| tuple_type  { EmlType.Tuple (List.rev $1) }
| arrow_type  { let (args, ret) = $1 in EmlType.Arrow (args, ret) }

arrow_type:
  simple_type ARROW simple_type { ([$1], $3) }
| simple_type ARROW arrow_type  { let (args, ret) = $3 in ($1 :: args, ret) }

tuple_type:
  simple_type STAR simple_type { [$3; $1] }
| tuple_type STAR simple_type  { $3 :: $1 }

simple_type:
  type_var                              { EmlType.genvar ?name:$1 () }
| UNIT                                  { EmlType.Unit }
| BOOL                                  { EmlType.Bool }
| CHAR                                  { EmlType.Char }
| INT                                   { EmlType.Int }
| FLOAT                                 { EmlType.Float }
| LIDENT                                { EmlType.Tconstr ($1, []) }
| simple_type LIDENT                    { EmlType.Tconstr ($2, [$1]) }
| LPAREN actual_type_args RPAREN LIDENT { EmlType.Tconstr ($4, List.rev $2) }
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
    { check_dup_args ~loc:(EmlLocation.from_rhs 2) $2;
      mk (Abs (List.rev $2, $4)) }
| IF expr THEN expr ELSE expr
  %prec prec_if
    { mk (If ($2, $4, $6)) }
| LET LIDENT EQUAL expr IN expr
  %prec prec_let
    { mk (Let (false, $2, $4, $6)) }
| LET LIDENT formal_args EQUAL expr IN expr
  %prec prec_let
    { check_dup_args ~loc:(EmlLocation.from_rhs 3) $3;
      mk (Let (false, $2, mk (Abs (List.rev $3, $5)), $7)) }
| LET REC LIDENT formal_args EQUAL expr IN expr
  %prec prec_let
    { check_dup_args ~loc:(EmlLocation.from_rhs 4) $4;
      mk (Let (true, $3, mk (Abs (List.rev $4, $6)), $8)) }
| MATCH expr WITH match_cases
  %prec prec_match
    { mk (Match ($2, List.rev $4)) }
| expr COLONCOLON expr                 { mk_exp_cons $1 $3 }
| expr ANDAND expr                     { mk (EmlOp (EmlOp.And ($1, $3))) }
| expr BARBAR expr                     { mk (EmlOp (EmlOp.Or ($1, $3))) }
| expr EQUAL expr                      { mk (EmlOp (EmlOp.Eq ($1, $3))) }
| expr LESS expr                       { mk (EmlOp (EmlOp.Lt ($1, $3))) }
| expr GREATER expr                    { mk (EmlOp (EmlOp.Gt ($1, $3))) }
| expr LESS_EQUAL expr                 { mk (EmlOp (EmlOp.Le ($1, $3))) }
| expr GREATER_EQUAL expr              { mk (EmlOp (EmlOp.Ge ($1, $3))) }
| expr LESS_GREATER expr               { mk (EmlOp (EmlOp.Ne ($1, $3))) }
| expr PLUS expr                       { mk (EmlOp (EmlOp.Add ($1, $3))) }
| expr MINUS expr                      { mk (EmlOp (EmlOp.Sub ($1, $3))) }
| expr STAR expr                       { mk (EmlOp (EmlOp.Mul ($1, $3))) }
| expr SLASH expr                      { mk (EmlOp (EmlOp.Div ($1, $3))) }
| expr MOD expr                        { mk (EmlOp (EmlOp.Mod ($1, $3))) }
| expr PLUSDOT expr                    { mk (EmlOp (EmlOp.FAdd ($1, $3))) }
| expr MINUSDOT expr                   { mk (EmlOp (EmlOp.FSub ($1, $3))) }
| expr STARDOT expr                    { mk (EmlOp (EmlOp.FMul ($1, $3))) }
| expr SLASHDOT expr                   { mk (EmlOp (EmlOp.FDiv ($1, $3))) }
| expr LAND expr                       { mk (EmlOp (EmlOp.Land ($1, $3))) }
| expr LOR expr                        { mk (EmlOp (EmlOp.Lor ($1, $3))) }
| expr LXOR expr                       { mk (EmlOp (EmlOp.Lxor ($1, $3))) }
| expr LSL expr                        { mk (EmlOp (EmlOp.Lsl ($1, $3))) }
| expr LSR expr                        { mk (EmlOp (EmlOp.Lsr ($1, $3))) }
| expr ASR expr                        { mk (EmlOp (EmlOp.Asr ($1, $3))) }
| NOT expr                             { mk (EmlOp (EmlOp.Not $2)) }
| LNOT expr                            { mk (EmlOp (EmlOp.Lnot $2)) }
| PLUSDOT expr  %prec prec_unary_plus  { mk (EmlOp (EmlOp.FPos $2)) }
| MINUSDOT expr %prec prec_unary_minus { mk (EmlOp (EmlOp.FNeg $2)) }
| PLUS expr     %prec prec_unary_plus  { mk_exp_unary_plus $2 }
| MINUS expr    %prec prec_unary_minus { mk_exp_unary_minus $2 }
| tuple_expr    %prec prec_tuple       { mk (Tuple (List.rev $1)) }

tuple_expr:
  expr COMMA expr       { [$3; $1] }
| tuple_expr COMMA expr { $3 :: $1 }

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
| LITERAL_BOOL                       { mk (Const (Bool $1)) }
| LITERAL_CHAR                       { mk (Const (Char $1)) }
| LITERAL_INT                        { mk (Const (Int $1)) }
| LITERAL_FLOAT                      { mk (Const (Float $1)) }
| LITERAL_STRING                     { mk_exp_string $1 }
| LIDENT                             { mk (Var $1) }
| LPAREN expr COLON type_expr RPAREN { mk (Constraint ($2, $4)) }
| LBRACKET RBRACKET                  { mk (Constr ("[]", [])) }
| LBRACKET exprs_semi RBRACKET       { mk_exp_list $2 }
| ERROR                              { mk Error }
| LPAREN expr RPAREN                 { $2 }
| BEGIN expr END                     { $2 }

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
| tuple_pattern
  %prec prec_tuple
    { mk (Ptuple (List.rev $1)) }

simple_pattern:
  UNDERSCORE                               { mk (Pvar None) }
| LIDENT                                   { mk (Pvar (Some $1)) }
| LPAREN RPAREN                            { mk (Pconst Punit) }
| LITERAL_BOOL                             { mk (Pconst (Pbool $1)) }
| LITERAL_CHAR                             { mk (Pconst (Pchar $1)) }
| LITERAL_INT                              { mk (Pconst (Pint $1)) }
| LITERAL_STRING                           { mk_pat_string $1 }
| UIDENT                                   { mk (Pconstr ($1, [])) }
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
