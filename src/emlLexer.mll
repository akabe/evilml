{
open EmlParser
open EmlUtils

let code_buf = Buffer.create 16

let get_code () =
  let n = Buffer.length code_buf in
  Buffer.sub code_buf 0 (n - 2) |> String.trim

let keyword_table = Hashtbl.create 53
let () = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
  [
    "begin", BEGIN;
    "end", END;
    "if", IF;
    "then", THEN;
    "else", ELSE;
    "true", LITERAL_BOOL true;
    "false", LITERAL_BOOL false;
    "let", LET;
    "rec", REC;
    "in", IN;
    "fun", FUN;
    "match", MATCH;
    "type", TYPE;
    "of", OF;
    "with", WITH;
    "not", NOT;
    "unit", UNIT;
    "bool", BOOL;
    "char", CHAR;
    "int", INT;
    "float", FLOAT;
    "mod", MOD;
    "land", LAND;
    "lor", LOR;
    "lxor", LXOR;
    "lnot", LNOT;
    "lsl", LSL;
    "lsr", LSR;
    "asr", ASR;
    "error", ERROR;
  ]

let make_ident lexbuf =
  let s = Lexing.lexeme lexbuf in
  try
    Hashtbl.find keyword_table s
  with Not_found ->
    if String.has_prefix "__ml_" s
    then errorf ~loc:(EmlLocation.from_lexbuf lexbuf)
        "Prefix `__ml_' is reserved: %s" s ()
    else if 'A' <= s.[0] && s.[0] <= 'Z' then UIDENT s else LIDENT s

let get_int lexbuf =
  let s = Lexing.lexeme lexbuf in
  try int_of_string s
  with _ -> errorf ~loc:(EmlLocation.from_lexbuf lexbuf)
              "Error: Illegal integer literal %s" s ()

let get_float lexbuf =
  let s = Lexing.lexeme lexbuf in
  try float_of_string s
  with _ -> errorf ~loc:(EmlLocation.from_lexbuf lexbuf)
              "Error: Illegal float literal %s" s ()

let get_quoted lexbuf =
  let s = Lexing.lexeme lexbuf in
  try
    String.sub s 1 (String.length s - 2)
    |> Scanf.unescaped
  with Scanf.Scan_failure msg ->
    errorf ~loc:(EmlLocation.from_lexbuf lexbuf)
      "Error: %s" msg ()
}

let bdigit = [ '0'-'1' ]
let odigit = [ '0'-'7' ]
let digit = [ '0'-'9' ]
let xdigit = [ '0'-'9' 'a'-'f' 'A'-'F' ]
let upper = [ 'A'-'Z' ]
let lower = [ 'a'-'z' ]
let sign = [ '+' '-' ]

let int_literal = digit+ | "0x" xdigit+ | "0b" bdigit+
let float_literal = digit+ ('.' digit*)? (['e' 'E'] sign? digit+)?
let char_literal = '\'' ([^ '\\' '\''] | '\\' _
                        | '\\' digit+
                        | "\\x" xdigit xdigit) '\''
let str_literal = '\"' ([^ '\\' '\"'] | '\\' _)* '\"'
let identifier = (upper | lower | '_') (digit | upper | lower | '_') *

rule main = parse
  [' ' '\t']     { main lexbuf }
| ['\n' '\r']    { Lexing.new_line lexbuf ; main lexbuf }
| "(*"           { comment lexbuf ; main lexbuf }
| "(*!"          { Buffer.clear code_buf ; cpp_code lexbuf ;
                   CPP_CODE (get_code ()) }
| "#use"         { HASH_USE }
| "&&"           { ANDAND }
| "||"           { BARBAR }
| '|'            { BAR }
| "<>"           { LESS_GREATER }
| "<="           { LESS_EQUAL }
| ">="           { GREATER_EQUAL }
| '='            { EQUAL }
| '<'            { LESS }
| '>'            { GREATER }
| "->"           { ARROW }
| "+."           { PLUSDOT }
| "-."           { MINUSDOT }
| "*."           { STARDOT }
| "/."           { SLASHDOT }
| '+'            { PLUS }
| '-'            { MINUS }
| '*'            { STAR }
| '/'            { SLASH }
| "::"           { COLONCOLON }
| ':'            { COLON }
| ';'            { SEMICOLON }
| ','            { COMMA }
| '('            { LPAREN }
| ')'            { RPAREN }
| '['            { LBRACKET }
| ']'            { RBRACKET }
| '_'            { UNDERSCORE }
| '\''           { QUOTE }
| int_literal    { LITERAL_INT (get_int lexbuf) }
| float_literal  { LITERAL_FLOAT (get_float lexbuf) }
| char_literal   { LITERAL_CHAR ((get_quoted lexbuf).[0]) }
| str_literal    { LITERAL_STRING (get_quoted lexbuf) }
| identifier     { make_ident lexbuf }
| eof            { EOF }
| _              { errorf ~loc:(EmlLocation.from_lexbuf lexbuf)
                     "Unknown token: %s" (Lexing.lexeme lexbuf) () }

and comment = parse
  "(*"           { comment lexbuf ; comment lexbuf }
| "*)"           { () }
| ['\n' '\r']    { Lexing.new_line lexbuf ; comment lexbuf }
| eof            { Format.eprintf "Warning: unterminated comment@." }
| _              { comment lexbuf }

and cpp_code = parse
  "(*"           { Buffer.add_string code_buf (Lexing.lexeme lexbuf) ;
                   cpp_code lexbuf ; cpp_code lexbuf }
| "*)"           { Buffer.add_string code_buf (Lexing.lexeme lexbuf) }
| ['\n' '\r']    { Lexing.new_line lexbuf ;
                   Buffer.add_string code_buf (Lexing.lexeme lexbuf) ;
                   cpp_code lexbuf }
| eof            { Format.eprintf "Warning: unterminated C++ code block@." }
| _              { Buffer.add_string code_buf (Lexing.lexeme lexbuf) ;
                   cpp_code lexbuf }
