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
    "true", VBOOL true;
    "false", VBOOL false;
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

let get_quoted lexbuf =
  let s = Lexing.lexeme lexbuf in
  String.sub s 1 (String.length s - 2)
  |> Scanf.unescaped
}

let digit = [ '0'-'9' ]
let odigit = [ '0'-'9' ]
let xdigit = [ '0'-'9' ]
let upper = [ 'A'-'Z' ]
let lower = [ 'a'-'z' ]
let sign = [ '+' '-' ]

let int_literal = digit+
let float_literal = digit+ ('.' digit*)? (['e' 'E'] sign? digit+)?
let char_literal = '\'' ([^ '\\' '\''] | '\\' _
                        | '\\' odigit odigit odigit
                        | "\\x" xdigit xdigit) '\''
let str_literal = '\"' ([^ '\\' '\"'] | '\\' _)* '\"'
let identifier = (upper | lower | '_') (digit | upper | lower | '_') *

rule main = parse
  [' ' '\t']     { main lexbuf }
| ['\n' '\r']    { Lexing.new_line lexbuf ; main lexbuf }
| "(*"           { comment lexbuf ; main lexbuf }
| "(*!"          { Buffer.clear code_buf ; cpp_code lexbuf ;
                   CPP_CODE (get_code ()) }
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
| int_literal    { VINT (int_of_string (Lexing.lexeme lexbuf)) }
| float_literal  { VFLOAT (float_of_string (Lexing.lexeme lexbuf)) }
| char_literal   { VCHAR ((get_quoted lexbuf).[0]) }
| str_literal    { VSTRING (get_quoted lexbuf) }
| identifier     { make_ident lexbuf }
| eof            { EOF }
| _              { errorf ~loc:(EmlLocation.from_lexbuf lexbuf)
                     "Unknown token: %s" (Lexing.lexeme lexbuf) () }

and comment = parse
  "(*"           { comment lexbuf ; comment lexbuf }
| "*)"           { () }
| eof            { Format.eprintf "Warning: unterminated comment@." }
| _              { comment lexbuf }

and cpp_code = parse
  "(*"           { Buffer.add_string code_buf (Lexing.lexeme lexbuf) ;
                   cpp_code lexbuf ; cpp_code lexbuf }
| "*)"           { Buffer.add_string code_buf (Lexing.lexeme lexbuf) }
| eof            { Format.eprintf "Warning: unterminated C++ code block@." }
| _              { Buffer.add_string code_buf (Lexing.lexeme lexbuf) ;
                   cpp_code lexbuf }
