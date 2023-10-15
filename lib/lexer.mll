{
  open Parser

  exception SyntaxError of string
}

let digit = ['0'-'9']
let integer = '-'? digit+

let ident = ['a'-'z' 'A'-'Z' '_']
let alpha = ['a'-'z' 'A'-'Z' '0'-'9' '_']

let newline = ['\n']
let skippable = [' ' '\t' '\r']

rule read = parse
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | skippable+ { read lexbuf }
  | "//" { read_comment lexbuf }
  | "()" { UNIT }
  | "struct" { STRUCT }
  | "type" { TYPE }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | ',' { COMMA }
  | ':' { COLON }
  | '(' { LPARENS }
  | ')' { RPARENS }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | "->" { ARROW }
  | '@' { LAMBDA }
  | "λ" { LAMBDA }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { ASTERISK }
  | '/' { SLASH }
  | '.' { DOT }
  | '=' { EQ }
  | '!' '=' { NEQ }
  | '>' { GT }
  | '>' '=' { GTE }
  | '<' { LT }
  | '<' '=' { LTE }
  | "true" { BOOL true }
  | "false" { BOOL false }
  | ident alpha * ['!' '?' '\'']*? as id { IDENT id }
  | integer as intg { INTEGER (int_of_string intg) }
  | '\'' ident alpha * as t { TYPEVAR t }
  | eof { EOF }
  | _ { raise @@ SyntaxError ("") }

and read_comment = parse
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | _ { read_comment lexbuf }
