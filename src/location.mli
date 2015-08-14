type position =
  {
    fname : string;
    lnum_start : int;
    cnum_start : int;
    lnum_end : int;
    cnum_end : int;
  }

type t = position option

val dummy : t
val from_position2 : Lexing.position -> Lexing.position -> t
val from_lexbuf : Lexing.lexbuf -> t
val from_symbol : unit -> t
val from_rhs : int -> t
val from_rhs2 : int -> int -> t
val pp : Format.formatter -> t -> unit

type 'a loc =
  {
    loc : t;
    data : 'a;
  }

val pp_loc :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a loc -> unit
val map : ('a -> 'b) -> 'a loc -> 'b loc
