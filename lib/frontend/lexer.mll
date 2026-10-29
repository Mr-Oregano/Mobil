{
  open Parser
  exception SyntaxError of string
}

let white = [' ' '\t' '\n' '\r']+
let comment = '#' [^ '\n' '\r']* 
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let numba = digit+
let ident = letter (letter | digit | '_')*

rule read = parse
  | white            { read lexbuf }
  | comment          { read lexbuf }
  | "if"             { KW_IF }
  | "then"           { KW_THEN }
  | "else"           { KW_ELSE }
  | "true"           { KW_TRUE }
  | "false"          { KW_FALSE }
  | "let"            { KW_LET }
  | "in"             { KW_IN }
  | "chan"           { KW_CHAN }
  | "marsh"          { KW_MARSH }
  | "marshal"        { KW_MARSHAL }
  | "unmarshal"      { KW_UNMARSHAL }
  | "num"            { KW_NUM }
  | "str"            { KW_STR }
  | "bool"           { KW_BOOL }
  | "unit"           { KW_UNIT }
  | "listen"         { KW_LISTEN }
  | "connect"        { KW_CONNECT }
  | "->"             { ARROW }
  | '('              { LPAREN }
  | ')'              { RPAREN }
  | '{'              { LBRACE }
  | '}'              { RBRACE }
  | '['              { LBRACK }
  | ']'              { RBRACK }
  | '!'              { EXCLAMATION }
  | '?'              { QUESTION }
  | ':'              { COLON }
  | '.'              { PERIOD }
  | ','              { COMMA }
  | '\\'             { BACKSLASH }
  | '+'              { PLUS }
  | '-'              { MINUS }
  | '*'              { STAR }
  | '/'              { SLASH }
  | '='              { EQUALS }
  | "()"             { UNIT }
  | "_"              { WILDCARD }
  | "\""             { STRING (read_string (Buffer.create 17) lexbuf) }
  | ident as id      { IDENT id }
  | numba as n       { NUM (int_of_string n) }
  | eof              { EOF }
  | _                { failwith "Unexpected character" }

and read_string buf = parse
  | '"'              { Buffer.contents buf }
  | [^ '"']+         { Buffer.add_string buf (Lexing.lexeme lexbuf);
                              read_string buf lexbuf }
  | eof              { raise (SyntaxError "String is not terminated") }