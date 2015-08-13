type position =
  {
    fname : string;
    lnum : int;
    bol : int;
    cnum_start : int;
    cnum_end : int;
  }

type t =
  | Dummy
  | Pos of position

let dummy = Dummy

let from_position2 p1 p2 =
  let open Lexing in
  Pos { fname = p1.pos_fname;
        lnum = p1.pos_lnum;
        bol = p1.pos_bol;
        cnum_start = p1.pos_cnum;
        cnum_end = p2.pos_cnum; }

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
  | Dummy -> ()
  | Pos p ->
    fprintf ppf "File %S, line %d, characters %d-%d:"
      p.fname p.lnum (p.cnum_start - p.bol) (p.cnum_end - p.bol)

type 'a loc =
  {
    loc : t;
    data : 'a;
  }

let pp_loc pp ppf l = pp ppf l.data
let map f l = { loc = l.loc; data = f l.data; }
