type position =
  {
    fname : string;
    lnum_start : int;
    cnum_start : int;
    lnum_end : int;
    cnum_end : int;
  }

type t = position option

let dummy = None

let from_position2 p1 p2 =
  let open Lexing in
  Some { fname = p1.pos_fname;
         lnum_start = p1.pos_lnum;
         cnum_start = p1.pos_cnum - p1.pos_bol;
         lnum_end = p2.pos_lnum;
         cnum_end = p2.pos_cnum - p2.pos_bol; }

let from_lexbuf lexbuf =
  from_position2 (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)

let from_symbol () =
  from_position2 (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ())

let from_rhs n =
  from_position2 (Parsing.rhs_start_pos n) (Parsing.rhs_end_pos n)

let from_rhs2 m n =
  from_position2 (Parsing.rhs_start_pos m) (Parsing.rhs_end_pos n)

let pp ppf =
  let open Format in
  function
  | None -> ()
  | Some p ->
    fprintf ppf "File %S, from line %d character %d, to line %d character %d"
      p.fname p.lnum_start p.cnum_start p.lnum_end p.cnum_end

type 'a loc =
  {
    loc : t;
    data : 'a;
  }

let pp_loc pp ppf l = pp ppf l.data
let map f l = { loc = l.loc; data = f l.data; }
